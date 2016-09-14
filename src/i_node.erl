-module(i_node).
-compile(export_all).
-record(state, {name,limit,reduce,done}).
-define(GEN_SERVER, [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export(?GEN_SERVER).

start_link(Name,Storage)                        -> gen_server:start_link(?MODULE, [Name,Storage], []).
init([api,Limit])                               -> i:cache(api,self()), {ok,#state{name=api,limit=Limit,reduce=0,done=0}};
init([worker,StartValue])                       -> gen_server:cast(self(),{next}), i:cache(StartValue,init),
                                                   {ok,#state{name=worker,limit=StartValue,reduce={StartValue,0}}}.
handle_info(_,S) -> {noreply,S}.
handle_call({reduce,R,N},_,#state{reduce=R1,done=Done,limit=Limit}=Pro) -> 
                    spawn(fun() -> supervisor:terminate_child(i,{worker,N}),
                                   supervisor:delete_child(i,{worker,N}) end),
                    [ gen_server:cast(i:cache(api),{start,Limit+I}) || I <- lists:seq(1,i:pool()-Limit+Done) ],

                  {reply,ok,Pro#state{limit=Limit+i:pool()-Limit+Done,reduce=erlang:max(R,R1),done=Done+1}};
handle_call({info},_,Proc)                      -> {reply,Proc,Proc}.
handle_cast({start,N},Proc)                     -> spawn(fun() -> supervisor:start_child(i,i:colz(N)) end), {noreply,Proc};
handle_cast({next}, #state{limit=S,reduce={1,L}}=State) -> i:cache(S,done), gen_server:call(i:cache(api),{reduce,L,S}), {noreply,State};
handle_cast({next}, #state{reduce={N,L}}=State) when N rem 2 == 0 -> gen_server:cast(self(),{next}), {noreply,State#state{reduce={N div 2,L+1}}};
handle_cast({next}, #state{reduce={N,L}}=State) -> gen_server:cast(self(),{next}), {noreply,State#state{reduce={3 *N +1,L+1}}}.
code_change(_, State, _)                        -> {ok, State}.
terminate(_Reason,State)                        -> ok.

collatz(1,L) -> L;
collatz(N,L) when N rem 2 == 0 -> collatz(N div 2,L+1);
collatz(N,L) -> collatz(3*N+1,L+1).

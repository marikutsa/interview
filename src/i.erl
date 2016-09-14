-module(i).
-behaviour(supervisor).
-behaviour(application).
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).
-export([start/2, stop/1, init/1]).
-record(core, { operation, resource, module, req, method }).
-define(POOL,1000).

dividers35() -> lists:sum([ case I rem 3 == 0 orelse I rem 5 == 0 of
                                 true -> I;
                                 false -> 0 end || I <- lists:seq(1,1000-1) ]).

pfactors() -> pfactors(600851475143,2).
pfactors(Num, F) when Num =< 1 -> F;
pfactors(Num, F) -> case Num rem F == 0 of
                        true ->  pfactors(Num div F, F);
                        false -> pfactors(Num, F + 1) end.

fibs() -> fibs(1,0,4000000).
fibs(A, B, M) when (A + B > M) -> 0;
fibs(A, B, M) -> Sum = (A + B),
                 case (Sum rem 2 == 0) of
                      true -> Sum + fibs(B, Sum, M);
                      false -> fibs(B, Sum, M) end.

palindroms() -> List = lists:seq(101, 999),
                lists:max([I*J || I <- List, J <- List,
                    integer_to_list(I*J) =:= lists:reverse(integer_to_list(I*J)) ]).

solve() -> solve(1).
solve(havenotime5min) -> 232792560;
solve(N) -> case lists:sum([ N rem I || I <- lists:seq(2,20) ]) == 0 of
                 true -> N;
                 false -> solve(N+1) end.

tables()   -> [ cache ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              { ok, { { one_for_one, 5, 10 }, [spec(8080),colz_api()] } }.

% curl -X POST -H 'Content-type: text/xml' -d @priv/Test.xml http://localhost:8080/capture

pool()     -> ?POOL.
colz(I)    -> {{worker,I},{i_node,start_link,[worker,I]},permanent,2000,worker,[i_node]}.
colz_api() -> {api,{i_node,start_link,[api,pool()]},permanent,2000,worker,[i_node]}.
stop(_)    -> ok.
start(_,_) -> Res = supervisor:start_link({local,i},i,[]),
              [ gen_server:cast(i:cache(api),{start,I}) || I <- lists:seq(1,pool() div 2)],
              Res.
spec(Port) -> ranch:child_spec(http, 100, ranch_tcp, port(Port), cowboy_protocol, env()).
env()      -> [ { env, [ { dispatch, points() } ] } ].
port(Port) -> [ { port, Port  } ].
points()   -> cowboy_router:compile([{'_', [ {"/capture", i, []} ]}]).

rest_init(Req, _Opts) ->
    {Method, Reqx}    = cowboy_req:method(Req),
    {ok, Reqx,  #core { module   = i,
                        method   = name(Method),
                        req      = Reqx }}.

name(<<"POST">>)    -> post.
allowed_methods(Req, State) -> {[ <<"POST">>, <<"GET">>, <<"HEAD">> ], Req, State}.
content_types_accepted(Req, State) -> {[{<<"text/xml">>, handle_xml_data}], Req, State}.
handle_xml_data(Req, #core{module = M, resource = Id} = State) ->
    {ok, Binary, Req2} = cowboy_req:body(Req),
    {Flag,Parsed} = handle_data(Binary, State),
    Req3 = cowboy_req:set_resp_body("OK\n",Req2),
    {Flag,Req3,State}.

init(_, _, _)                                               -> {upgrade, protocol, cowboy_rest}.
resource_exists(Req,  #core{module = M } = S)               -> { true, Req, S}.
handle_data(Data,   #core{module = M, method=Method} = S) -> M:Method(Data,S).

post(Data,State) ->
    {Root, _} = xmerl_scan:string(binary_to_list(Data)),
    List = xmerl_xpath:string("//BaseAttributeValues", Root),
    Res = lists:map(fun(I=#xmlElement{}) -> [ begin
                 [{_,K},{_,V}] = [ {Name,Value} || #xmlAttribute{name=Name,value=Value} <- Attrs,
                 Name == baseAttrId orelse Name == value ], {K,unicode:characters_to_list(V)} end
                 || #xmlElement{attributes=Attrs} <- I#xmlElement.content ] end,List),
    Res3 = [ begin
             Attrs = filter(A,[],{0,0}),
             Values = [ proplists:get_value(X,Attrs,[]) || X <- ['GTIN','NAME','DESC','COMP'] ],
             file:write_file("priv/test.csv",
                 unicode:characters_to_binary(lists:flatten(io_lib:format("~ts,~ts,~ts,~ts~n",Values))),[append]),
             Attrs end || A <- Res, filter(A,[],{0,0}) /= [] ],
    {true,[]}.

filter([],Attrs,{1,1}) -> Attrs;
filter([],Attrs,{_,_}) -> [];
filter([{"PROD_COVER_GTIN",V} =X|Rest],Attrs,{A,B}) -> filter(Rest,[{'GTIN',V}|Attrs],{1,B});
filter([{"PROD_NAME",V}       =X|Rest],Attrs,{A,B}) -> filter(Rest,[{'NAME',V}|Attrs],{A,1});
filter([{"PROD_DESC",V}       =X|Rest],Attrs,{A,B}) -> filter(Rest,[{'DESC',V}|Attrs],{A,B});
filter([{"BRAND_OWNER_NAME",V}=X|Rest],Attrs,{A,B}) -> filter(Rest,[{'COMP',V}|Attrs],{A,B});
filter([X|Rest],Attrs,{A,B}) -> filter(Rest,Attrs,{A,B}).

main(A) -> mad:main(A).

% i:validate({gtin,"04250021234506"}).

validate({gtin,Value}) when is_binary(Value) -> validate({gtin,binary_to_list(Value)});
validate({gtin,Value}) when is_list(Value) andalso (length(Value) == 18 orelse
                                                    length(Value) == 17 orelse
                                                    length(Value) == 14 orelse
                                                    length(Value) == 13 orelse
                                                    length(Value) == 12 orelse
                                                    length(Value) == 8) ->
    [Control|Sum]   = lists:reverse(Value),
    [_,Multipliers] = lists:foldl(fun(X,[3,L]) -> [1,[X*3|L]];
                                     (X,[1,L]) -> [3,[X|L]] end,[3,[]],Sum),
    Digit = $0 + 10 - lists:sum(Multipliers) rem 10,
    case Digit of Control -> ok;
                        _ -> {error,{checksum,Digit-Control}} end;
validate(_)               -> {error,gtin}.

cache(Key, undefined) -> ets:delete(cache,Key);
cache(Key, Value) -> ets:insert(cache,{Key,Value}), Value.
cache(Key) ->
    Res = ets:lookup(cache,Key),
    Val = case Res of [] -> undefined; [Value] -> Value; Values -> Values end,
    case Val of undefined -> undefined;
                {_,X} -> X;
                _ -> Val end.

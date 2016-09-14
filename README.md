Erlang Interview Questions and Answers
======================================

* 1-5 — Project Euler
* 6 — XML over REST sample
* 7 — Massive Spawn
* 8 — GTIN validator
* SQL — sample SQL

Compile
-------

```
$ git clone http://github.com/5HT/interview && cd interview
$ ./mad dep com pla rep
```

Test
----

### 1. Dividers Below 1000

```
> i:dividers35().
233168
```

### 2. Prime Factors of 600851475143

```
> i:pfactors().
233168
```

### 3. Even Fib Sum Below 4000000

```
> i:fibs().
4613732
```

### 4. Palindroms from 101 to 999

```
> i:palindroms().
906609
```

### 5. Dividers Below 20

```
> i:solve().
232792560
```

### 6. XML over REST sample

```
$ curl -X POST -H 'Content-type: text/xml' -d @priv/Test.xml http://localhost:8080/capture
$ cat priv/test.csv
```

### 7. Massive Spawn

While running test on 1M numbers a bit timeless, you can observe current calculations as follow:

```
> gen_server:call(i:cache(api),{info}).
{state,api,1001455,524,1000456}
17> length(supervisor:which_children(i)).
999
18> supervisor:which_children(i).
[{{worker,233690},<0.22658.21>,worker,[i_node]},
 {{worker,233689},<0.22655.21>,worker,[i_node]},
 {{worker,233688},<0.22652.21>,worker,[i_node]},
 {{worker,233666},<0.22583.21>,worker,[...]},
 {{worker,233665},<0.22580.21>,worker,...},
 {{worker,...},<0.22577.21>,...},
 {{...},...},
 {...}|...]
```

Current max length of sequence 524 of first 1000456 natural numbers.
Note that CPU avg is about 70% with a pool of 1000 processes.

### 8. GTIN validator

```
> i:validate({gtin,"04250021234506"}).
ok
7> i:validate({gtin,"04250021234502"}).
{error,{checksum,4}}
```

### 9. SQL sample

```
initdb -D /usr/local/var/postgres
createdb `whoami`
psql -d `whoami` -f init.sql
```

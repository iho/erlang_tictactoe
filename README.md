# erlang_tictactoe


```
erl -name node1@127.0.0.1 -setcookie game
erl -name node2@127.0.0.1 -setcookie game
erl -name node3@127.0.0.1 -setcookie game

c(tictactoe).

and 

tictactoe:start_link(). in each node
```


(c) Ihor Horobets
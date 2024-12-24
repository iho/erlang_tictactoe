-module(tictactoe).

-behaviour(gen_server).

-define(BOARD_SIZE, 3).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([make_move/3, is_game_over/2, is_winner/2]).
-export([substitute/4, check_columns/1, check_rows/1, check_diagonals/1, loop_game/2]).

start_link() ->
    net_kernel:connect_node('node1@127.0.0.1'),
    net_kernel:connect_node('node2@127.0.0.1'),
    net_kernel:connect_node('node3@127.0.0.1'),

    Nodes = erlang:nodes(),
    io:format("Nodes: ~p~n", [Nodes]),

    Suffix = rand:uniform(100),
    PlayerName = list_to_binary("player" ++ integer_to_list(Suffix)),
    io:format("Player name: ~p~n", [PlayerName]),
    {_, ServerPid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format("Server pid ~p ~n", [ServerPid]),
    global:register_name(PlayerName, ServerPid),
    {ok, ServerPid}.

%% Helper Function
schedule_work() ->
    %% Schedule a `work` message after 1000 milliseconds (1 second)
    erlang:send_after(1000, self(), work).

%% Genserver callbacks
init(Board) ->
    % generete suffix for the player name
    schedule_work(),
    {ok, Board}.

handle_call({ask_for_number, _}, _From, Board) ->
    % decide who plays first
    rand:seed(default),
    Random = rand:uniform(100),
    {reply, Random, Board};
handle_call({make_move, Symbol, InBoard}, _From, _Board) ->
    NewBoard = play_turn(InBoard, Symbol),
    io:format("NewBoard: ~p~n", [NewBoard]),
    {reply, NewBoard, NewBoard};
handle_call(get_board, _From, Board) ->
    {reply, Board, Board}.

handle_cast({make_first_move, Pid}, _Board) ->
    NewBoard = play_game(x),
    io:format("First move made.~n"),
    io:format("Board: ~p~n", [NewBoard]),

    Result = loop_game(NewBoard, Pid),
    io:format("Response: ~p~n", [Result]),
    {noreply, NewBoard};
handle_cast(reset, _Board) ->
    {noreply, []};
handle_cast(_Msg, Board) ->
    {noreply, Board}.

loop_game(ok, _) ->
    ok;
loop_game(Board, Pid) ->
    io:format("Board: ~p~n", [Board]),
    io:format("Player pid: ~p~n", [Pid]),
    Response = gen_server:call(Pid, {make_move, y, Board}),
    % Response = play_turn(Board, y),
    % io:format("Response: ~p~n", [Response]),
    NewBoard2 = play_turn(Response, x),

    io:format("NewBoard: ~p~n", [NewBoard2]),
    loop_game(NewBoard2, Pid).

handle_info(work, State) ->
    %% Perform the periodic task
    io:format("Periodic task executed at ~p~n", [erlang:now()]),

    Nodes = global:registered_names(),
    io:format("Nodes: ~p~n", [Nodes]),
    if length(Nodes) == 2 ->
           io:format("Both players are registered.~n"),
           Player1 = lists:nth(1, Nodes),
           Player2 = lists:nth(2, Nodes),
           io:format("Player1: ~p, Player2: ~p~n", [Player1, Player2]),
           if Player1 == Player2 ->
                  io:format("Both players are the same.~n"),
                  ok;
              true ->
                  ok
           end,

           Player1Pid = global:whereis_name(Player1),
           Player2Pid = global:whereis_name(Player2),

           if self() == Player1Pid ->
                  gen_server:cast(Player2Pid, {make_first_move, Player1Pid});
              true ->
                  gen_server:cast(Player1Pid, {make_first_move, Player2Pid})
           end;
       true ->
           io:format("Not all players are registered.~n")
    end,
    %% Schedule the next execution
    schedule_work(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

substitute(Row, Col, Player, Board) ->
    substitute(Row, Col, Player, Board, 1, []).

substitute(_, _, _, [], _, Acc) ->
    lists:reverse(Acc); % End of the board, return the result
substitute(Row, Col, Player, [CurrentRow | Rest], RowIndex, Acc) ->
    if RowIndex =:= Row ->
           % Replace the specific column in the target row
           UpdatedRow = replace_in_row(Col, Player, CurrentRow),
           substitute(Row, Col, Player, Rest, RowIndex + 1, [UpdatedRow | Acc]);
       true ->
           % Keep the row unchanged
           substitute(Row, Col, Player, Rest, RowIndex + 1, [CurrentRow | Acc])
    end.

replace_in_row(Col, NewValue, Row) ->
    replace_in_row(Col, NewValue, Row, 1, []).

replace_in_row(_, _, [], _, Acc) ->
    lists:reverse(Acc);
replace_in_row(Col, NewValue, [_ | Tail], Col, Acc) ->
    lists:reverse(Acc) ++ [NewValue | Tail];
replace_in_row(Col, NewValue, [Head | Tail], CurrentCol, Acc) ->
    replace_in_row(Col, NewValue, Tail, CurrentCol + 1, [Head | Acc]).

make_move(Board, {Row, Col}, Player) ->
    Res = lists:nth(Row, lists:nth(Col, Board)),
    io:format("Res: ~p~n", [Res]),
    case Res of
        empty ->
            substitute(Row, Col, Player, Board);
        _ ->
            Board
    end.

is_game_over(Board, Player) ->
    is_winner(Board, Player)
    orelse is_winner(lists:transpose(Board), Player)
    orelse is_winner(diagonals(Board), Player).

diagonals(Board) ->
    [diagonal(Board, 1, 1, 1), diagonal(Board, 1, ?BOARD_SIZE, -1)].

diagonal(Board, Row, Col, Inc) ->
    case lists:nth(Row, lists:nth(Col, Board)) of
        Player when Player == x; Player == o ->
            [Player | diagonal(Board, Row + 1, Col + Inc, Inc)];
        _ ->
            []
    end.

is_winner(Board, Player) ->
    lists:any(fun(Row) -> Row == [Player, Player, Player] end, Board).

play_game(First) ->
    GameBoard = empty_board(?BOARD_SIZE),
    io:format("Empty game board: ~p~n", [GameBoard]),
    play_turn(GameBoard, x).

empty_board(Size) ->
    lists:duplicate(Size, lists:duplicate(Size, empty)).

play_turn(Board, Symbol) ->
    io:format(" Symbol: ~p~n", [Symbol]),
    case check_winner(Board) of
        none ->
            Move = generate_move(Board, 0),
            case Move of
                {no_move, _} ->
                    io:format("No move.~n"),
                    ok;
                _ ->
                    UpdatedBoard = make_move(Board, Move, Symbol),
                    io:format(" made move ~p with symbol ~p.~n", [Move, Symbol]),
                    UpdatedBoard
            end;
        Winner ->
            io:format("Game over. Winner: ~p~n", [Winner])
    end.

% Board = [[empty,empty,empty],[empty,empty, empty],[empty,empty,empty]].
%
%
generate_move(Board, Counter) when Counter < 10 ->
    Row = rand:uniform(?BOARD_SIZE),
    Col = rand:uniform(?BOARD_SIZE),
    io:format("Row: ~p, Col: ~p, Board ~p ~n", [Row, Col, Board]),
    case lists:nth(Row, lists:nth(Col, Board)) of
        empty ->
            {Row, Col};
        _ ->
            generate_move(Board, Counter + 1)
    end;
generate_move(_, _) ->
    {no_move, no_move}.

check_winner(Board) ->
    io:format("Checking winner.~n"),
    io:format("Board: ~p~n", [Board]),
    case check_rows(Board) of
        x ->
            x;
        o ->
            o;
        _ ->
            check_columns(Board)
    end.


check_columns(ok) ->
    ok;
check_columns(Board) ->
    check_columns(Board, 1).

check_columns(Board, Col) ->
    case check_column(Board, Col) of
        x ->
            x;
        o ->
            o;
        _ ->
            case Col of
                ?BOARD_SIZE ->
                    none;
                _ ->
                    check_columns(Board, Col + 1)
            end
    end.

transpose([[] | _]) ->
    [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

check_column(Board, Col) ->
    case lists:nth(Col, transpose(Board)) of
        [x, x, x] ->
            x;
        [o, o, o] ->
            o;
        _ ->
            none
    end.
check_rows(ok) ->
    ok; 
check_rows(Board) ->
    io:format("Checking rows.~n"),
    io:format("Board: ~p~n", [Board]),
    % check rows 1 2 3 for winner
    case check_rows(Board, 1) of
        x ->
            x;
        o ->
            o;
        _ ->
            case check_rows(Board, 2) of
                x ->
                    x;
                o ->
                    o;
                _ ->
                    check_rows(Board, 3)
            end
    end.

check_rows(Board, Row) ->
    case check_row(Board, Row) of
        x ->
            x;
        o ->
            o;
        _ ->
            case Row of
                ?BOARD_SIZE ->
                    check_diagonals(Board);
                _ ->
                    check_rows(Board, Row + 1)
            end
    end.

check_row(Board, Row) ->
    case lists:nth(Row, Board) of
        [x, x, x] ->
            x;
        [o, o, o] ->
            o;
        _ ->
            none
    end.

check_diagonals(Board) ->
    case {check_main_diagonal(Board), check_anti_diagonal(Board)} of
        {true, _} ->
            true;
        {_, true} ->
            true;
        _ ->
            false
    end.

% Check the main diagonal (top-left to bottom-right)
check_main_diagonal(Board) ->
    [A, _, _] = lists:nth(1, Board),
    [_, B, _] = lists:nth(2, Board),
    [_, _, C] = lists:nth(3, Board),
    A == B andalso B == C andalso A /= empty.

% Check the anti-diagonal (top-right to bottom-left)
check_anti_diagonal(Board) ->
    [_, _, A] = lists:nth(1, Board),
    [_, B, _] = lists:nth(2, Board),
    [C, _, _] = lists:nth(3, Board),
    A == B andalso B == C andalso A /= empty.

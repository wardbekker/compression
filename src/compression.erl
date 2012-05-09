%%%-------------------------------------------------------------------
%%% @author Ward Bekker <ward@tty.nl>
%%% @copyright (C) 2012, Ward Bekker
%%% @doc
%%% Provides D-GAP and Elias Gamma encoding/decoding which can be used
%%% for Inverted Index Compression
%%% @end
%%%-------------------------------------------------------------------
-module(compression).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export(
   [encode/1, decode/1, encode_gamma/1,
    decode_gamma/1, encode_gaps/1, decode_gaps/1]
  ).

%% TYPES
-type positive_integer_vector() :: [positive_integer()].
-type positive_integer_list() :: [positive_integer()].
-type positive_integer() :: integer().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Combined Elias Gamma & D-GAPS encoding. 
-spec encode(positive_integer_vector()) -> bitstring().
encode([]) ->
    <<>>;
encode(Integers) when is_list(Integers) ->
    encode_gamma(encode_gaps(Integers)).

%% @doc Combined Elias Gamma & D-GAPS decoding. 
-spec decode(bitstring()) -> positive_integer_vector().
decode(<<>>) ->
    [];
decode(Stream) when is_bitstring(Stream) ->
    decode_gaps(decode_gamma(Stream)).

%% @doc D-GAP encoding 
-spec encode_gaps(positive_integer_vector()) -> positive_integer_list().
encode_gaps(Integers) when is_list(Integers) ->
    { Gaps, TailVal, _Int } = lists:foldl(
                               fun (Int, { Gaps, TailVal, LastInt }) ->
                                       Diff = Int - LastInt,
                                       case Diff of
                                           1 -> 
                                               { Gaps, TailVal + 1, Int };
                                           _ ->
                                               {  [Gaps, [TailVal, Diff - 1]], 1, Int}
                                       end
                               end,
                               { [], 0, -1 },
                               Integers
                              ),
    lists:flatten([Gaps,[TailVal]]).

%% @doc D-GAP decoding 
-spec decode_gaps(positive_integer_list()) -> positive_integer_vector().
decode_gaps([]) ->
    [];
decode_gaps(Integers) when is_list(Integers) ->
    { _, Results, _ } = lists:foldl(
      fun(I, { Gap, Results, LastResult}) ->
              NewLastResult = LastResult + I,
              case I of
                  0 -> { true, [], 0 };
                  _ -> case Gap of
                             false -> 
                                 { true, [Results, lists:seq(LastResult, NewLastResult - 1)], NewLastResult };
                             true ->
                                 { false, Results, NewLastResult }
                         end
              end
      end,
      { false, [], 0 },
      Integers
     ),
    lists:flatten(Results).

%% @doc Elias Gamma encoding, supporting zero
-spec encode_gamma(positive_integer_vector() | positive_integer() ) -> bitstring().
encode_gamma(Integers) when is_list(Integers) ->
    list_to_bitstring([encode_gamma(I) || I <- Integers]);
encode_gamma(Integer) when is_integer(Integer) ->
    BitSize = length(integer_to_list(Integer + 1,2)),
    <<0:(BitSize - 1), (Integer+1):BitSize>>.

%% @doc Elias Gamma decoding, supporting zero
-spec decode_gamma(bitstring()) -> positive_integer_vector().
decode_gamma(<<>>) ->
    [];
decode_gamma(Stream) ->
    { N, Stream1 } = take_zeroes(Stream),
    BitSize = N+1,
    <<Int:BitSize, Stream2/bitstring>> = Stream1,
    [Int-1] ++ decode_gamma(Stream2).

%%%===================================================================
%%% Internal functions
%%%===================================================================

take_zeroes(<<>>) ->
    { 0, <<>> };
take_zeroes(<<0:1,Stream/bitstring>>) ->
    { Zeroes, Stream1 } = take_zeroes( Stream ),
    { Zeroes + 1, Stream1 };
take_zeroes(Stream) ->
    { 0, Stream }.

-ifdef(TEST).

%%%===================================================================
%%% PropEr quickcheck tests
%%%===================================================================

proper_test_() ->
    [{atom_to_list(F),
      fun () -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].

prop_gamma_encoding() ->
    ?FORALL(T, vector(pos_integer()), T =:= decode_gamma(encode_gamma(T))).

prop_gaps_encoding() ->
    ?FORALL(T, vector(pos_integer()), T =:= decode_gaps(encode_gaps(T))).

prop_combined_encoding() ->
    ?FORALL(T, vector(pos_integer()), T =:= decode(encode(T))).

vector(Type) ->
     ?LET(UN, list(Type), lists:usort(UN)).

-endif.


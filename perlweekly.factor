USING:
  arrays assocs assocs.extras
  calendar calendar.format calendar.parser
  circular
  combinators combinators.short-circuit.smart
  english
  grouping
  hash-sets
  io
  kernel
  literals
  make
  math math.combinatorics math.functions math.matrices
  math.order math.parser math.statistics math.vectors
  path-finding
  prettyprint
  random ranges
  sets sequences sequences.extras sequences.product
  sorting sorting.specification splitting strings
  unicode
  vectors
;
IN: perlweekly

! -- 246 --

: 6-of-49 ( -- )
  49 [1..b] >vector randomize
  6 [ dup pop . ] times
  drop
;

: (linear-recurrence-2nd-order?) ( a[n] a[n-2] a[n-1] -- ? )
  ! a[n] = p * a[n-2] + q * a[n-1]
  ! oh, math?
  ! Sorry, not implementing this for now.
  3drop f
;

: linear-recurrence-2nd-order? ( a -- ? )
  2 4 [a..b] [           ! a | n
    [ 2 - ] [ 1 + ] bi   ! a | n-2 n+1
    pick subseq first3   ! a | a[n-2] a[n-1] a[n]
    -rot (linear-recurrence-2nd-order?)
  ] all? nip
;

! -- 247 --

: >surname ( name -- surname )
  " " split1 nip
;

: preferably-not ( seq surname -- name )
  '[ >surname _ = ]     ! seq match?
  dupd reject           ! seq seq'
  [ nip ] unless-empty  ! seq/seq'
  random                ! name
;

: (secret-santa) ( seq giver -- recvr )
  swap dupd remove swap  ! seq' giver
  >surname               ! seq' giver-surname
  preferably-not         ! recvr
;

: secret-santa ( names -- )
  dup [                      ! names | giver
    dup " -> " append write  ! names | giver
    dupd (secret-santa)      ! names | recvr
    dup write nl             ! names | recvr
    swap remove              ! names'
  ] each drop                !
;

: most-frequent-letter-pair ( str -- str' )
  2 clump sorted-histogram
  dup last last
  '[ _ = ] filter-values
  keys infimum
;

! -- 248 --

: shortest-distance ( i str char -- dist )
  -rot                      ! char i str
  [ drop nip ]              ! i
  [ index-from ]            ! i1
  [ swap head last-index ]  ! i2
  3tri                      ! i i1 i2
  2array sift               ! i {i1,i2}
  swap '[ _ - abs ] map     ! {d1,d2}
  infimum                   ! d1/d2
;

: shortest-distances ( str char -- distances )
  dupd '[ _ _ shortest-distance ]  ! str [ str char shortest-distance ]
  [ length [0..b) ] dip            ! [0..11] [ str char shortest-distance ]
  map                              ! distances
;

: corner-sum ( row col matrix -- n/f )
  [
    [ dup 1 + [a..b] ] bi@
    2array <product-sequence>
  ] dip
  matrix-nths sum
;

: sum-matrix ( matrix -- m )
  dup dimension
  [ 1 - ] map first2
  [ pick corner-sum ] <matrix-by-indices>
  nip
;

! -- 249 --

: equal-pairs ( seq -- pairs )
  sort 2 group
  dup [ first2 = ] all?
  [ drop { } ] unless
;

: di-match? ( s perm i -- ? )  ! s perm i
  [ pick nth ] keep            ! s perm char i
  [ pick nth ] keep            ! s perm char x i
  1 + reach nth                ! s perm char x y
  > CHAR: D CHAR: I ? =        ! s perm t/f
  2nip                         ! t/f
;

: di-match ( s -- seq )       ! "IDID"
  dup length                  ! "IDID" 4
  [ [0..b] ] [ [0..b) ] bi    ! "IDID" [0..4] [0..3]
  '[ _                        ! "IDID" perm0 [0..3]
    [                         ! "IDID" perm0 i
      [ 2dup ] dip di-match?  ! "IDID" perm0 t/f
    ] all? nip                ! "IDID" t/f
  ] find-permutation          ! "IDID" perm
  nip                         ! perm
;

! -- 250 --

: smallest-index ( seq -- i )
  [ 10 mod = ] find-index drop  ! i/f
  [ -1 ] unless* ;

: alphanum>val ( str -- n )
  dup string>number [ nip ] [ length ] if* ;

: alphanum-max ( seq -- n )
  [ alphanum>val ] map-supremum ;

! -- 251 --

: (ccval) ( seq subtotal -- seq' subtotal' )    ! { 6 12 25 1 } 0 --or-- { 2 } 110
  over length 1 = [                             ! { 2 } 110
    [ unclip ] dip +                            ! { } 112
  ] [                                           ! { 6 12 25 1 } 0
    swap unclip-last                            ! 0 { 6 12 25 } 1
    [ unclip ] dip                              ! 0 { 12 25 } 6 1
    [ number>string ] bi@ append string>number  ! 0 { 12 25 } 61
    swapd +                                     ! { 12 25 } 61
  ] if                                          ! { 12 25 } 61 --or-- { } 112
;

: ccval ( seq -- n )                   ! { 6 12 25 1 }
  0 [ over empty? ] [ (ccval) ] until  ! { } 1286
  nip                                  ! 1286
;

: lucky? ( m coord -- ? )       ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } } { 2 0 }
  2dup first swap row infimum   ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } } { 2 0 } 15
  2over last swap col supremum  ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } } { 2 0 } 15 15
  over = [                      ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } } { 2 0 } 15
    spin matrix-nth =           ! t
  ] [ 3drop f ] if              ! f
;

: get-lucky ( m -- n )                   ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } }
  dup dimension                          ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } } { 3 3 }
  [ [0..b) ] map first2                  ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } } [0..2] [0..2]
  [ 2array dupd lucky? ] cartesian-find  ! { { 3 7 8 } { 9 11 13 } { 15 16 17 } } 2 0
  dup [
    2array swap matrix-nth               ! 15
  ] [ 3drop -1 ] if ;

! -- 252 --

: special-numbers ( seq -- n )
  dup length '[ nip 1 + _ swap divisor? ] filter-index
  sum-of-squares ;

: unique-sum-zero ( n -- seq )
  [ { } ] [ [1..b) dup sum -1 * suffix ] if-zero ;

! -- 253 --

: split-strings ( strings char -- words )  ! { "one.two.three" "four.five" "six" } '.'
  1string '[ _ split ] map-concat harvest ;

: row-compare ( i j m -- <=> )
  [ 2dup 2array ] dip    ! i j { i j } m
  rows first2            ! i j ri rj
  [ [ 1 = ] count ] bi@  ! i j #r1 #j1
  <=>                    ! i j +lt+/+eq+/+gt+
  {
    { +lt+ [ 2drop +lt+ ] }
    { +gt+ [ 2drop +gt+ ] }
    { +eq+ [ <=> ] }
  } case ;

: weakest-rows ( m -- seq )
  [ length [0..b) ]
  [ '[ _ row-compare ] ]
  bi sort-with ;

! -- 254 --

: cubish? ( n -- ? )
  abs
  3 swap nth-root
  1 mod
  zero? ;

: reverse-vowels ( str -- str' )
  [ >lower [ vowel? ] find-all ]
  [ clone ] bi                  ! idx-vowels str
  over keys [                   ! idx-vowels str | idx
    2dup nth-of 1string upper?  ! idx-vowels str | idx t/f
    reach pop last swap         ! idx-vowels str | idx vowel t/f
    [ ch>upper ] when           ! idx-vowels str | idx vowel
    set-nth-of                  ! idx-vowels str'
  ] each nip ;

: reverse-vowels-2 ( str -- str' )
  [ clone ] [
    >lower [ vowel? ] find-all
    [ values reverse ] [ keys ] bi
  ] bi                       ! str vowels idxs
  [                          ! str | vowel idx
    pick dupd nth            ! str | vowel idx orig
    1string upper?           ! str | vowel idx t/f
    swapd [ ch>upper ] when  ! str | idx vowel
    set-nth-of               ! str'
  ] 2each ;

! -- 255 --

: odd-character ( s t -- 1str )  ! "Perl" "Preel"
  ! [ >lower ] bi@
  swap [                         ! "Preel" | 'P'
    over index                   ! "Preel" | 0/f
    [ remove-nth-of ] when*      ! "reel"  |
  ] each                         ! "e"
;

: most-frequent-word ( paragraph banned-word -- most-frequent-word )
  ! [ >lower ] bi@
  swap ",.!:?" without
  split-words remove
  mode ;

! -- 256 --
! Not sure I interpreted the task correctly

: max-pairs ( words -- n )
  dup '[
    [ reverse _ in? ]
    [ cardinality 1 > ] bi and
  ] filter

  histogram

  dup '[
    over reverse _ at min
  ] assoc-map

  sum-values 2 / ;

: merge-strings ( str1 str2 -- str )
  [ >array ] bi@
  zip-longest concat sift
  >string ;

! -- 257 --

: smaller-than-current ( ints -- n )
  dup [
    over [ over < ] count nip
  ] map nip ;

MEMO: leading-1 ( row -- i/f )
  [ zero? not ] find  ! i/f n/f
  1 number=           ! i/f t/f
  [ drop f ] unless ;

MEMO: all-zeros? ( seq -- ? )
  [ zero? ] all? ;

: (rre-1?) ( m -- ? )
  [ { [ all-zeros? ] [ leading-1 ] } || ] all? ;

: (rre-2?) ( m -- ? )
  dup [ all-zeros? ] count
  tail* [ all-zeros? ] all? ;

: (rre-3?) ( m -- ? )
  [
    [ leading-1 ] bi@
    2dup and [ < ] [ 2drop t ] if
  ] monotonic? ;

: (rre-4?) ( m -- ? )
  [ [ leading-1 ] map-sift ]
  [ cols ]
  [ length 1 - ] tri  ! cols n
  '[ [ zero? ] count _ number= ] all? ;

: reduced-row-echelon? ( m -- ? )
  { [ (rre-1?) ] [ (rre-2?) ] [ (rre-3?) ] [ (rre-4?) ] } && ;

! -- 258 --

: count-even-digits-number ( seq -- n )
  [ log10 >integer odd? ] count ;

MEMO: binary-rep-has-k-ones? ( int k -- ? )
  swap 2 >base [ CHAR: 1 = ] count number= ;

: sum-of-values ( ints k -- n )
  [ dup length [0..b) ]
  [ '[ _ binary-rep-has-k-ones? ] ]
  bi* filter nths-of sum ;

! -- 259 --

: banking-days ( start-date end-date weekday-exceptions -- n )
  [ [ weekdays-between ] 2keep ] dip  ! n start end | holidays
  -rot '[ _ _ between? ]              ! n holidays [within-dates?]
  count - ;

: more-days-needed ( start end needed-workdays weekday-exceptions -- n )
  swap [ banking-days ] dip
  swap - ;

: banking-day-offset ( ymd offset holidays -- ymd )
  [ ymd>timestamp ] map [ weekend? ] reject  ! ymd offset weekday-exceptions

  [ ymd>timestamp ]                   ! start | offset
  [ [ dupd days time+ ] keep 1 + ]    ! start end needed-workdays | weekday-exceptions
  [ '[ _ _ more-days-needed ] ] tri*  ! start end [more-days-needed']

  '[ 2dup @ dup zero? [ drop t ] [ days time+ f ] if ] [ ] until  ! start end'

  nip timestamp>ymd ;

! TODO: part 2

! -- 260 --

: unique-occurrences ( ints -- 1/0 )
  histogram values all-unique?
  1 0 ? ;

: dict-rank ( word -- n )
  >upper dup
  all-unique-permutations sort
  index 1 + ;

! -- 261 --

: element-digit-sum ( ints -- sum )
  [ sum ] [
    [ number>string ] map-concat
    [ 1string string>number ] map-sum
  ] bi - abs ;

: multiply-by-two ( ints start -- n )
  dup zero? [ nip ] [
    2dup index*
    [ 2 * multiply-by-two ] [ nip ] if
  ] if ;

! -- 262 --

: max-positive-negative ( ints -- n )
  [ neg? ] partition
  [ zero? ] reject
  [ length ] bi@ max ;

: (count-equal-divisible) ( i k ints -- n )  ! i k ints
    pick dupd nth-of                         ! i k ints ints[i]
    indices*                                 ! i k js
    pick '[ _ > ] filter                     ! i k js*
    -rot '[ _ * _ divisor? ] count ;

: count-equal-divisible ( ints k -- n )  ! ints k
  swap                                   ! k ints
  [ '[ _ _ (count-equal-divisible) ] ]
  [ length [0..b) ] bi                   ! ( i -- n ) range
  swap map-sum ;

! -- 263 --

: target-index ( ints k -- indices )
  swap sort indices ;

: single-merge ( items -- merged-items )
  [ first ] collect-by
  [ sum-values ] assoc-map ;

: merge-items ( items1 items2 -- items )
  [ single-merge ] bi@
  [ + ] assoc-merge ;

! -- 264 --

: greatest-english-letter ( str -- str' )
  dup [
    { [ 1string upper? ] [ ch>lower over in? ] } &&
  ] filter nip
  dup empty? [ supremum 1string ] unless ;

: target-array ( source indices -- target )
  [ V{ } clone ] 2dip
  [ pick insert-nth! ] 2each ;

! -- 265 --

: 33%-appearance ( ints -- n/f )
  [ sorted-histogram ] [ length .33 * ] bi
  '[ nip _ >= ] assoc-find 2drop ;

: at-least? ( str char n -- ? )
  -rot '[ _ = ] count <= ;

: at-least-all? ( str histogram -- ? )
  [ [ dup ] 2dip at-least? ] assoc-all? nip ;

: completing-word ( str completions -- smallest-completion )
  swap >lower [ letter? ] filter histogram
  '[ _ at-least-all? ] filter
  [ "" ] [ shortest ] if-empty ;

! -- 266 --

: singles ( seq -- seq' )
  histogram
  [ nip 1 = ] assoc-filter
  keys ;

: uncommon-words ( line1 line2 -- seq )
  [ split-words ] bi@
  2dup swap
  [ singles swap diff ] 2bi@ append
  [ { "" } ] when-empty ;

: nonzero-diagonals? ( m -- ? )
  [ main-diagonal ] [ anti-diagonal ] bi
  [ [ zero? ] none? ] bi@ and ;

: zero-diagonals ( m -- m' )
  [ 0 set-nth-of ] map-index
  [ [ dup length 1 - ] dip - 0 set-nth-of ] map-index ;

: x-matrix? ( m -- ? )
  dup nonzero-diagonals? [
    zero-diagonals
    [ [ zero? ] all? ] all?
  ] [ drop f ] if ;

! -- 267 --

: product-sign ( ints -- -1/1/0 )
  dup [ zero? ] any?
  [ drop 0 ] [
    [ neg? ] count even?
    1 -1 ?
  ] if ;

: line-widths ( str-widths -- line-widths )
  { } swap
  [ + ] 0accumulate
  [
    [ 100 > ] cut-when
    [ last ] dip
    over '[ _ - ] map
    [ suffix ] dip
  ] until-empty ;

: str-widths ( str alphabet-widths -- str-widths )
  "abcdefghijklmnopqrstuvwxyz" swap zip
  '[ _ at ] { } map-as ;

: line-counts ( str alphabet-widths -- num-lines last-width )
  str-widths line-widths
  [ length ] [ last ] bi ;

! -- 268 --

: magic-number ( seq-x seq-y -- n )
  [ supremum ] bi@ swap - ;

: number-game ( ints -- ints' )
  sort 2 group
  [ reverse ] map-concat ;

! -- 269 --

: bitwise-or? ( ints -- t/f )
  [ 0 bit? not ] count
  2 >= ;

: (d-e-prepare) ( ints -- remaining vec1 vec2 )
  2 cut swap
  1 cut
  [ >vector ] bi@ ;

: (d-e-which-seq) ( seq1 seq2 -- seq1/seq2 )
  [ [ last ] bi@ > ] 2keep ? ;

: distribute-elements ( ints -- ints' )
  (d-e-prepare)              ! remaining vec1 vec2
  2dup '[ _ _ ] 3dip '[ _ _  ! vec1 vec2 remaining  | vec1 vec2
    (d-e-which-seq)          ! vec1 vec2 remaining  | vec
    [ unclip ] dip swap      ! vec1 vec2 remaining' | vec n
    suffix! drop
  ] until-empty append ;

! -- 270 --

: special? ( i j m -- ? )
  {
    [ [ 2array ] dip matrix-nth 1 = ]
    [ nip row [ 1 = ] count 1 = ]
    [ swapd nip col [ 1 = ] count 1 = ]
  } && ;

: special-positions ( m -- n )
  0 swap

  [ dimension [ <iota> ] map first2 ]
  [ '[ _ special? [ 1 + ] when ] ] bi

  2nested-each ;

: target-indices ( ints -- indices )
  dup maximum
  '[ _ < ] find-all [ first ] map
  2 ?cut drop ;

: plan-level ( x y indices -- x/y indices' )
  dup length {
    { 0 [ 2nip 0 swap ] }
    { 1 [ nip ] }
    { 2 [ 2over 2 / < [ 1 cut drop nip ] [ swapd nip ] if ] }
  } case ;

: cheap-move ( ints x y -- ints' cost )
  pick target-indices plan-level  ! ints x/y indices'
  pick [ 1 + ] change-nths ;

: min-cost ( ints x y -- n )
  '[ _ _ cheap-move ]         ! ints cheap-move'
  [ 0 ] 2dip                  ! cost ints cheap-move'
  [ call '[ _ + ] dip ] keep  ! cost' ints' cheap-move'
  pick swap '[                ! cost ints cost
    drop @
    dup '[ _ + ] 2dip
  ] until-zero drop ;

! -- 271 --

: maximum-ones ( matrix -- row-num )
  [ sum ] map
  [ maximum ] [ index ] bi
  1 + ;

: int-compare ( int1 int2 -- <=> )
  [ >bin [ CHAR: 1 = ] count ] compare ;

: sort-by-1bits ( ints -- ints' )
  { { int-compare } { <=> } } sort-with-spec ;

! -- 272 --

: defang ( ipv4 -- defanged )
  "." "[.]" replace ;

: string-score ( str -- n )
  2 clump
  [ first2 - abs ] map-sum ;

! -- 273 --

: char-percentage ( str char -- n )
  '[ _ = ] percent-of 100 * round ;

: b-after-a? ( str -- ? )
  dup CHAR: b index*
  dup [ swap CHAR: a index-from* not ] [ nip ] if ;

! -- 274 --

: >goat-latin ( sentence -- sentence' )
  split-words
  [
    swap
    dup first ch>lower vowel? [
      unclip suffix
    ] unless
    "ma" append
    swap 1 + CHAR: a <string> append
  ] map-index " " join ;

! TODO: part 2

! -- 275 --

: broken-keys ( sentence keys -- n )
  [ >lower ] bi@
  swap split-words
  [ intersects? not ] with count ;

CONSTANT: LETTERS $[ CHAR: a CHAR: z [a..b] <circular> ]

: ch+ ( n ch -- ch' )
  LETTERS index +
  LETTERS nth ;

: prev-letter ( str i -- ch )
  head [ letter? ] find-last nip ;

: replace-digits ( str -- str' )
  >lower dup
  [
    over digit? [
      pick swap prev-letter
      [ 1string string>number ] dip ch+
    ] [ drop ] if
  ] map-index nip ;

! -- 276 --

: complete-days ( hours -- n )
  2 all-combinations
  [ first2 - 24 divisor? ] count ;

: maximum-frequency ( ints -- n )
  histogram values
  dup maximum
  '[ _ = ] filter sum ;

! -- 277 --

: count-common ( words1 words2 -- n )
  [
    histogram
    [ 1 = ] filter-values
    keys
  ] bi@
  intersect length ;

: strong-pairs ( ints -- n )
  2 all-combinations
  [ cardinality 1 = ] reject
  [ >hash-set ] map
  >hash-set members
  [ members ] map
  [
    [ first2 - abs ]
    [ minimum ]
    bi <
  ] count ;

! -- 278 --

: sort-string ( shuffled -- sorted )
  ! Assumes no earlier "stray" digits in words
  split-words
  [
    [ digit? ] cut-when
    string>number
    2array
  ] map
  sort-values
  keys
  " " join
;

: reverse-word ( word char -- reversed )
  2dup swap in? [
    1string [ split1 ] keep
    '[ _ append sort ] dip
    "" append-as
  ] [ drop ] if
;

! -- 279 --

: sort-letters ( letters weights -- letters' )
  zip sort-values keys >string ;

: split-string? ( str -- ? )
  >lower [ vowel? ] count even? ;

! does 'y' count? Let's say no, for now.

! -- 280 --

: twice-appearance ( str -- char )
  dup members >array
  [ over indices ?second ] map
  ?minimum nth-of ;

: count-asterisks ( str -- n )
  "|" split <evens> concat
  [ CHAR: * = ] count ;

! -- 281 --

: square-is-light? ( str -- ? )
  sum odd? ;

CONSTANT: CHESS-INDICES $[
  7 [0..b] dup 2array <product-sequence>
]

CONSTANT: KNIGHT-MOVES $[
  { 1 -1 } 2 all-selections
  [ { 2 1 } v* ] map
  dup [ reverse ] map append
]

: knight-neighbors ( pair -- neighbor-pairs )
  KNIGHT-MOVES [ v+ ] with map
  CHESS-INDICES intersect ;

CONSTANT: ALL-KNIGHT-NEIGHBORS $[
  [
    CHESS-INDICES [ [ knight-neighbors ] keep ,, ] each
  ] H{ } make
]

CONSTANT: KNIGHT-BFS $[
  ALL-KNIGHT-NEIGHBORS <bfs>
]

: pos>row-col ( str -- pair )
  [ last 49 - ] [ first 97 - ] bi 2array ;

: knight-min-moves ( startpos endpos -- n )
  [ pos>row-col ] bi@
  KNIGHT-BFS find-path
  length 1 - ;

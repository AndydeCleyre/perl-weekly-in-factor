USING:
  arrays assocs assocs.extras
  calendar calendar.format calendar.parser
  combinators combinators.short-circuit.smart
  english
  grouping
  io
  kernel
  math math.combinatorics math.functions math.matrices math.order math.parser math.statistics
  prettyprint
  random ranges
  sets sequences sequences.extras sequences.product sorting splitting strings
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
  [ -1 ] unless*
;

: alphanum>val ( str -- n )
  dup string>number [ nip ] [ length ] if*
;

: alphanum-max ( seq -- n )
  [ alphanum>val ] map-supremum
;

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
  ] [ 3drop -1 ] if
;

! -- 252 --

: special-numbers ( seq -- n )
  dup length '[ nip 1 + _ swap divisor? ] filter-index
  sum-of-squares
;

: unique-sum-zero ( n -- seq )
  [ { } ] [ [1..b) dup sum -1 * suffix ] if-zero
;

! -- 253 --

: split-strings ( strings char -- words )  ! { "one.two.three" "four.five" "six" } '.'
  1string '[ _ split ] map-concat harvest
;

: row-compare ( i j m -- <=> )
  [ 2dup 2array ] dip    ! i j { i j } m
  rows first2            ! i j ri rj
  [ [ 1 = ] count ] bi@  ! i j #r1 #j1
  <=>                    ! i j +lt+/+eq+/+gt+
  {
    { +lt+ [ 2drop +lt+ ] }
    { +gt+ [ 2drop +gt+ ] }
    { +eq+ [ <=> ] }
  } case
;

: weakest-rows ( m -- seq )
  [ length [0..b) ]
  [ '[ _ row-compare ] ]
  bi sort-with
;

! -- 254 --

: cubish? ( n -- ? )
  abs
  3 swap nth-root
  1 mod
  zero?
;

: reverse-vowels ( str -- str' )
  [ >lower [ vowel? ] find-all ]
  [ clone ] bi                  ! idx-vowels str
  over keys [                   ! idx-vowels str | idx
    2dup nth-of 1string upper?  ! idx-vowels str | idx t/f
    reach pop last swap         ! idx-vowels str | idx vowel t/f
    [ ch>upper ] when           ! idx-vowels str | idx vowel
    set-nth-of                  ! idx-vowels str'
  ] each nip                    ! str'
;

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
  ] 2each                    ! str'
;

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
  mode
;

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

  sum-values 2 /
;

: merge-strings ( str1 str2 -- str )
  [ >array ] bi@
  zip-longest concat sift
  >string
;

! -- 257 --

: smaller-than-current ( ints -- n )
  dup [
    over [ over < ] count nip
  ] map nip
;

MEMO: leading-1 ( row -- i/f )
  [ zero? not ] find  ! i/f n/f
  1 number=           ! i/f t/f
  [ drop f ] unless   ! i/f
;

MEMO: all-zeros? ( seq -- ? )
  [ zero? ] all?
;

: (rre-1?) ( m -- ? )
  [ { [ all-zeros? ] [ leading-1 ] } || ] all?
;

: (rre-2?) ( m -- ? )
  dup [ all-zeros? ] count
  tail* [ all-zeros? ] all?
;

: (rre-3?) ( m -- ? )
  [
    [ leading-1 ] bi@
    2dup and [ < ] [ 2drop t ] if
  ] monotonic?
;

: (rre-4?) ( m -- ? )
  [ [ leading-1 ] map-sift ]
  [ cols ]
  [ length 1 - ] tri  ! cols n
  '[ [ zero? ] count _ number= ] all?
;

: reduced-row-echelon? ( m -- ? )
  { [ (rre-1?) ] [ (rre-2?) ] [ (rre-3?) ] [ (rre-4?) ] } &&
;

! -- 258 --

: count-even-digits-number ( seq -- n )
  [ log10 >integer odd? ] count
;

MEMO: binary-rep-has-k-ones? ( int k -- ? )
  swap 2 >base [ CHAR: 1 = ] count number=
;

: sum-of-values ( ints k -- n )
  [ dup length [0..b) ]
  [ '[ _ binary-rep-has-k-ones? ] ]
  bi* filter nths-of sum
;

! -- 259 --

: banking-days ( start-date end-date weekday-exceptions -- n )
  [ [ weekdays-between ] 2keep ] dip  ! n start end | holidays
  -rot '[ _ _ between? ]              ! n holidays [within-dates?]
  count -                             ! n
;

: more-days-needed ( start end needed-workdays weekday-exceptions -- n )
  swap [ banking-days ] dip
  swap -
;

: banking-day-offset ( ymd offset holidays -- ymd )
  [ ymd>timestamp ] map [ weekend? ] reject  ! ymd offset weekday-exceptions

  [ ymd>timestamp ]                   ! start | offset
  [ [ dupd days time+ ] keep 1 + ]    ! start end needed-workdays | weekday-exceptions
  [ '[ _ _ more-days-needed ] ] tri*  ! start end [more-days-needed']

  '[ 2dup @ dup zero? [ drop t ] [ days time+ f ] if ] [ ] until  ! start end'

  nip timestamp>ymd
;

! TODO: part 2

! -- 260 --

: unique-occurrences ( ints -- 1/0 )
  histogram values all-unique?
  1 0 ?
;

: dict-rank ( word -- n )
  >upper dup
  all-unique-permutations sort
  index 1 +
;

! -- 261 --

: element-digit-sum ( ints -- sum )
  [ sum ] [
    [ number>string ] map-concat
    [ 1string string>number ] map-sum
  ] bi - abs
;

: multiply-by-two ( ints start -- n )
  dup zero? [ nip ] [
    2dup index*
    [ 2 * multiply-by-two ] [ nip ] if
  ] if
;

! -- 262 --

: max-positive-negative ( ints -- n )
  [ [ neg? ] count ]
  [ [ 0 > ] count ] bi
  max
;

: max-positive-negative-2 ( ints -- n )
  [ neg? ] partition
  [ zero? ] reject
  [ length ] bi@ max
;

: (count-equal-divisible) ( k ints i -- n )  ! k ints i
    dupd [ nth-of ] keep                     ! k ints ints[i] i
    [ indices* ] dip                         ! k js i
    dup '[ [ _ > ] filter ] dip              ! k js* i
    rot '[ _ * _ divisor? ] count            ! n
;

: count-equal-divisible ( ints k -- n )   ! ints k
  0 spin                                  ! sum k ints
  dup length [0..b) [                     ! sum k ints | i
    [ 2dup ] dip (count-equal-divisible)  ! sum k ints | n
    '[ _ + ] 2dip                         ! sum* k ints
  ] each 2drop                            ! sum*
;

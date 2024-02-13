USING:
  arrays assocs assocs.extras
  combinators
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

: di-match? ( s perm i -- ? )       ! s perm i
  [ pick nth ] keep                 ! s perm char i
  [ pick nth ] keep                 ! s perm char x i
  1 + reach nth                     ! s perm char x y
  > [ CHAR: D = ] [ CHAR: I = ] if  ! s perm t/f
  2nip                              ! t/f
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
    dupd swap reverse _ at min
  ] assoc-map

  sum-values 2 /
;

: merge-strings ( str1 str2 -- str )
  [ zip "" concat-as ]
  [
    2array [ length ] zip-with inv-sort-values
    [ first first ]
    [ [ first ] [ last ] bi [ last ] bi@ - ] bi
    tail*
  ] 2bi
  append
;

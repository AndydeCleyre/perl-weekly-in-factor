USING: arrays grouping kernel math
math.combinatorics math.functions math.matrices math.parser
math.statistics ranges sequences sequences.extras
sequences.product sorting ;
IN: perlweekly

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

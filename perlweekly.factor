USING: arrays kernel math math.functions math.matrices
math.parser math.statistics ranges sequences sequences.extras ;
IN: perlweekly

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

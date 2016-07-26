public
atLeast: lowerLimit
	"Answer the next good size >= lowerlimit.
	If lowerLimit is larger than the largest known good prime,
	just make it odd."

	| primes low mid high prime |
	primes := self sizes.
	low := 1.
	high := primes size.
	lowerLimit > (primes at: high)
		ifTrue:
			[ ^ lowerLimit even
				ifTrue: [ lowerLimit + 1 ]
				ifFalse: [ lowerLimit ] ].
	[ mid := (high - low) // 2 + low.
	prime := primes at: mid.
	prime < lowerLimit
		ifTrue: [ low := mid ]
		ifFalse: [ high := mid ].
	high - low <= 1
		ifTrue: [ ^ primes at: high ] ] repeat
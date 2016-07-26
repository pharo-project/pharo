testing
isMostLikelyPrime
	"See isProbablyPrimeWithK:andQ: for the algoritm description."
	| k q |
	self <= 1 ifTrue: [^self error: 'operation undefined'].
	self even ifTrue: [^self = 2].
	k := 1.

	q := self - 1 bitShift: -1.
	[q odd] whileFalse: 
			[q := q bitShift: -1.
			k := k + 1].
	
	25 timesRepeat: [(self isProbablyPrimeWithK: k andQ: q) ifFalse: [^false]].
	^true
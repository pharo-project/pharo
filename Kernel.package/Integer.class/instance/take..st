mathematical functions
take: kk
	"Return the number of combinations of (self) elements taken kk at a time.  
	For 6 take 3, this is 6*5*4 / (1*2*3).  Zero outside of Pascal's triangle.  Use a trick to go faster."
	" 6 take: 3  "

	| num denom |
	kk < 0 ifTrue: [^ 0].
	kk > self ifTrue: [^ 0].
	num := 1.
	self to: (kk max: self-kk) + 1 by: -1 do: [:factor | num := num * factor].
	denom := 1.
	1 to: (kk min: self-kk) do: [:factor | denom := denom * factor].
	^ num // denom
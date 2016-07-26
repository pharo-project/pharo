comparing
<= aNumber
	aNumber class = self class ifTrue: [^self asFraction <= aNumber asFraction].
	^self asFraction <= aNumber
arithmetic
/ aNumber
	"Answer the result of dividing the receiver by aNumber."
	aNumber isFraction
		ifTrue: [^self * aNumber reciprocal].
	^ aNumber adaptToFraction: self andSend: #/
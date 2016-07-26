private
optimizeSet: aSet
	"If a set is small, convert it to array to speed up lookup
	(Array has no hashing overhead, beats Set on small number
	of elements)."

	^aSet size < 10 ifTrue: [aSet asArray] ifFalse: [aSet]
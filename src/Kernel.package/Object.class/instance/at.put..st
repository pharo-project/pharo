accessing
at: index put: value 
	"Primitive. Assumes receiver is indexable. Store the argument value in 
	the indexable element of the receiver indicated by index. Fail if the 
	index is not an Integer or is out of bounds. Or fail if the value is not of 
	the right type for this kind of collection. Answer the value that was 
	stored. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [(index between: 1 and: self size)
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
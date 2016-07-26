accessing
at: index put: aCharacter
	"Store the Character into the field of the receiver indicated by the index.
	 Primitive.  Fail if the index is not an Integer or is out of bounds, or if the
	 argument is not a Character.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 64>
	^aCharacter isCharacter
		ifTrue:
			[index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse:
			[self errorImproperStore]
attributes
getAttribute: anAttributeClass

	^self style get: anAttributeClass ifAbsent: [ text defaultStyle get: anAttributeClass ] 
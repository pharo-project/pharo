private
isUnknownCapitalizedMessage: aSelector
	^ aSelector first isUppercase and: [ aSelector isSelectorSymbol not ]
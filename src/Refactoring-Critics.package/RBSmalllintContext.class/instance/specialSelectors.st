private
specialSelectors
	| answer |
	answer := IdentityDictionary new.
	(Smalltalk specialSelectors select: [:sel | sel isSymbol]) do:
		[:sel | answer at: sel put: nil.].
	^answer.
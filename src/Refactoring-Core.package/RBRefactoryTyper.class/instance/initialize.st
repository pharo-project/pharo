initialization
initialize
	model := RBClassModelFactory rbNamespace new.
	class := model classFor: Object.
	variableTypes := Dictionary new.
	variableMessages := Dictionary new.
	selectorLookup := IdentityDictionary new.
	bestGuesses := Dictionary new
tests - dictionary including
testIncludesComportementForDictionnary
	| valueIn collection keyIn |
	collection := self nonEmpty.
	valueIn := collection values anyOne.
	keyIn := collection keys anyOne.
	self assert: (collection includes: valueIn).
	self deny: (collection includes: self valueNotInNonEmpty).
	" testing that includes take only care of values :"
	self deny: (collection includes: keyIn)
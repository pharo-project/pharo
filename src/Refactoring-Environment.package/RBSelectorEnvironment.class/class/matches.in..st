instance creation
matches: aString in: anEnvironment 
	| newEnvironment |
	newEnvironment := (self onEnvironment: anEnvironment)
		label: 'Matching: ' , aString;
		searchStrings: (Array with: aString);
		yourself.
	anEnvironment classesAndSelectorsDo: [ :class :selector | 
		| method |
		method := class compiledMethodAt: selector.
		method allLiterals do: [ :literal | 
			literal isString ifTrue: [
				(aString match: literal)
					ifTrue: [ newEnvironment addClass: class selector: selector ] ] ] ].
	^ newEnvironment
instance creation
referencesTo: aLiteral in: anEnvironment 
	| classDict literalPrintString |
	literalPrintString := aLiteral isVariableBinding
		ifTrue: [ aLiteral key asString ]
		ifFalse: [
			aLiteral isString
				ifTrue: [ aLiteral ]
				ifFalse: [ aLiteral printString ] ].
	classDict := IdentityDictionary new.
	anEnvironment classesDo: [ :class | 
		| selectors |
		selectors := (class thoroughWhichSelectorsReferTo: aLiteral)
			select: [ :selector | anEnvironment includesSelector: selector in: class ].
		selectors isEmpty 
			ifFalse: [ classDict at: class put: selectors asIdentitySet ] ].
	^ (self onEnvironment: anEnvironment)
		on: classDict;
		label: 'References to: ' , literalPrintString;
		searchStrings: (Array with: literalPrintString);
		yourself
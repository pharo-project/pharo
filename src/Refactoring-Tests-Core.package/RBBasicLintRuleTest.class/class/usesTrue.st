bugs
usesTrue
	| detector trueBinding falseBinding |
	detector := self new.
	trueBinding := Smalltalk associationAt: #True.
	falseBinding := Smalltalk associationAt: #False.
	detector name: 'Uses True/False instead of true/false'.
	detector methodBlock: 
			[:context :result | 
			| method |
			method := context compiledMethod.
			((method referencesLiteral: trueBinding)
				or: [method referencesLiteral: falseBinding]) ifTrue: 
						[result addClass: context selectedClass selector: context selector.
						result searchStrings: #('True' 'False')]].
	^detector
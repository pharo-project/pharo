process list
prettyNameForProcess: aProcess 
	| nameAndRules |
	aProcess ifNil: [ ^'<nil>' ].
	nameAndRules := self nameAndRulesFor: aProcess.
	^ aProcess browserPrintStringWith: nameAndRules first
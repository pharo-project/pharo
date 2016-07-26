initialize-release
readSelectorFrom: aPragma argumentNames: argumentNames argumentValues: argumentValues
	| index |
	index := argumentNames
		indexOf: 'selector:'
		ifAbsent: [ ^ aPragma selector ].
	
	^ argumentValues at: index
initialize-release
readIsSourceFrom: aPragma argumentNames: argumentNames argumentValues: argumentValues
	| index |
	index := argumentNames
		indexOf: 'isSource:'
		ifAbsent: [ ^ false ].
	
	^ argumentValues at: index
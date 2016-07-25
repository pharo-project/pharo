transforming
processReferenceNode: aNode 
	| varName |
	varName := aNode name.
	ignore = varName ifTrue: [^self].
	(aNode whoDefines: varName) notNil ifTrue: [^self].
	(self instanceVariableNames includes: varName) 
		ifTrue: [instVarReaders add: varName].
	(self classVariableNames includes: varName) 
		ifTrue: [classVarReaders add: varName]
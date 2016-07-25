transforming
renameConflictingTemporary: aName 
	| allNames newName index seqNode |
	allNames := (Set new)
				addAll: inlineParseTree allDefinedVariables;
				yourself.
	allNames remove: aName ifAbsent: [].
	seqNode := sourceMessage.
	[seqNode isSequence] whileFalse: [seqNode := seqNode parent].
	allNames addAll: seqNode allDefinedVariables.	"Add those variables defined in blocks. This might cause a few 
													variables to be renamed that don't need to be, but this should be safe."
	newName := aName.
	index := 0.
	
	[(sourceMessage whoDefines: newName) notNil or: 
			[(class hierarchyDefinesVariable: newName) or: [allNames includes: newName]]] 
			whileTrue: 
				[index := index + 1.
				newName := aName , index printString].
	newName = aName ifFalse: [self renameTemporary: aName to: newName].
	^newName
tranforming
transform
	| definingNode variableNode |
	parseTree := class parseTreeFor: selector.
	variableNode := self 
				whichVariableNode: parseTree
				inInterval: interval
				name: oldName.
	(variableNode isNil or: [variableNode isVariable not]) 
		ifTrue: [self refactoringFailure: oldName , ' isn''t a valid variable'].
	variableNode name = oldName 
		ifFalse: [self refactoringFailure: 'Invalid selection'].
	definingNode := variableNode whoDefines: oldName.
	definingNode isNil 
		ifTrue: [self refactoringFailure: oldName , ' isn''t defined by the method'].
	self renameNode: definingNode.
	class compileTree: parseTree
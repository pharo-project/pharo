Extension { #name : 'RBRenameInstanceVariableRefactoring' }

{ #category : '*Epicea-Refactorings' }
RBRenameInstanceVariableRefactoring >> asEpiceaEvent [

	^ EpRenameInstanceVariableRefactoring rename: self variableName to: self newName in: self class name
]

{ #category : '*Epicea-Refactorings' }
RBRenameInstanceVariableRefactoring >> propagateTransformation [
	self renameReferences
]

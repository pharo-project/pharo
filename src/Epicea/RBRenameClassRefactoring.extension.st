Extension { #name : #RBRenameClassRefactoring }

{ #category : #'*Epicea-Refactorings' }
RBRenameClassRefactoring >> asEpiceaEvent [
	^ EpRenameClassRefactoring 
		rename: className
		to: newName
]

{ #category : #'*Epicea-Refactorings' }
RBRenameClassRefactoring >> propagateTransformation [
	self renameReferences
]

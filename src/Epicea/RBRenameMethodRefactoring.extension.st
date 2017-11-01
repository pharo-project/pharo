Extension { #name : #RBRenameMethodRefactoring }

{ #category : #'*Epicea-Refactorings' }
RBRenameMethodRefactoring >> asEpiceaEvent [
	^ EpRenameMethodRefactoring 
		renameMethod: oldSelector 
		in: class name
		to: newSelector
]

{ #category : #'*Epicea-Refactorings' }
RBRenameMethodRefactoring >> propagateTransformation [
	self renameMessageSends
]

Extension { #name : #RBRenameVariableChange }

{ #category : #'*NautilusRefactoring' }
RBRenameVariableChange >> nameToDisplay [

	^ 'Rename ', self oldName, ' into ', self newName
]

{ #category : #'*NautilusRefactoring' }
RBRenameVariableChange >> textToDisplay [

	self printString
]

{ #category : #'*NautilusRefactoring' }
RBRenameVariableChange >> whatToDisplayIn: aChangeBrowser [

	| result |
	result := OrderedCollection with: self.
	^ result, (self changes flatCollect: [:e | e changes ])
]

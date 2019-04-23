Extension { #name : #RBRenameVariableChange }

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

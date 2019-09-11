Extension { #name : #RBVariableNode }

{ #category : #'*NECompletion' }
RBVariableNode >> completionToken [

	^ self name
]

{ #category : #'*NECompletion' }
RBVariableNode >> narrowWith: aString [

 	self name: aString asSymbol
]

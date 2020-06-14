Extension { #name : #RBLiteralNode }

{ #category : #'*RewriteTool' }
RBLiteralNode >> isInTemporariesList [
	^ false
]

{ #category : #'*RewriteTool' }
RBLiteralNode >> optionsForNode [
	| options |
	options := super optionsForNode.
	options
		add: #('Abstract literal' #abstractLiteral);
		add: #('Abstract object' #abstractMessage);
		yourself.
	^ options
]

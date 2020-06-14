Extension { #name : #RBReturnNode }

{ #category : #'*RewriteTool' }
RBReturnNode >> optionsForNode [
	| options |
	options := super optionsForNode.
	options add: #('Abstract object' #abstractMessage).
	^ options
]

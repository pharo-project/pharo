Extension { #name : #RBBlockNode }

{ #category : #'*RewriteTool' }
RBBlockNode >> optionsForNode [
	| options |
	options := super optionsForNode.
	options add: #('Abstract block' #abstractBlock).
	^ options
]

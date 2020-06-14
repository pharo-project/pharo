Extension { #name : #RBAssignmentNode }

{ #category : #'*RewriteTool' }
RBAssignmentNode >> optionsForNode [
	| options |
	options := super optionsForNode.
	options add: #('Abstract statement' #abstractStatement).
	^ options
]

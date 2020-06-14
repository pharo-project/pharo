Extension { #name : #RBPatternVariableNode }

{ #category : #'*RewriteTool' }
RBPatternVariableNode >> optionsForNode [
	"not sure if i need as list and recurse into..."

	| options |
	options := OrderedCollection new.
	self oldNodes ifNotEmpty: [ options add: #('Undo on node' #undoOnNode) ].
	(self recurseInto or: [ self isList not ])
		ifFalse: [ options add: #('Recurse into' #recurseInto) ].
	options add: #('Change' #change).
	^ options
]

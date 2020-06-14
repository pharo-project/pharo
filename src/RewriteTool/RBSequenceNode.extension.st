Extension { #name : #RBSequenceNode }

{ #category : #'*RewriteTool' }
RBSequenceNode >> isInTemporariesList [
	^ true
]

{ #category : #'*RewriteTool' }
RBSequenceNode >> optionsForNode [
	| options |
	Halt now.
	options := super optionsForNode.
	self temporaries isNotEmpty
		ifTrue:
			[ options add: #('Abstract temporaries list' #abstractTemporariesList) ].
	options add: #('Abstract statements' #abstractStatements).
	^ options
]

initialization
initialize
	super initialize.
	self matcher
		matches: '| `@temps | ``@.stmts'
		do: [ :sequence :answer |
			answer isNil
				ifFalse: [ answer ]
				ifTrue: [
					sequence temporaries
						detect: [ :temp |
							| isRead isWritten |
							isRead := false.
							isWritten := false.
							sequence statements do: [ :statement | 
								statement nodesDo: [ :node |
									(node isVariable and: [ node name = temp name ]) ifTrue: [
										isRead := isRead or: [ node isUsed ].
										isWritten := isWritten or: [ node isWrite ] ] ] ].
							(isRead and: [ isWritten ]) not ]
						ifNone: [ nil ] ] ]
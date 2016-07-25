visiting
visitCascadeNode: aCascadeNode
	self isInCascadeNode: true.
	self visitNode: aCascadeNode receiver.
	self
		indentAround: [ 
			self newLineBeforeFirstCascade
				ifTrue: [ self newLine ]
				ifFalse: [ self space ].
			aCascadeNode messages
				do: [ :each | 
					self
						indentAround: [ 
							self formatCommentsFor: each.
							self
								formatSelectorAndArguments: each
								firstSeparator: [  ]
								restSeparator:
									((self isMultiLineMessage: each)
										ifTrue: [ [ self newLine ] ]
										ifFalse: [ [ self space ] ]) ] ]
				separatedBy: [ 
					codeStream nextPut: $;.
					self newLineAfterCascade
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ] ].
	self isInCascadeNode: false
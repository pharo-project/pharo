private
formatTemporariesFor: aSequenceNode
	aSequenceNode temporaries isEmpty
		ifTrue: [ ^ self ].
	self
		bracketWith: '|'
		around:
			[ 
			self space.
			aSequenceNode temporaries
				do:
					[ :each | 
					self visitNode: each.
					self formatStatementCommentsFor: each .
					self space ] ].
	self newLine.
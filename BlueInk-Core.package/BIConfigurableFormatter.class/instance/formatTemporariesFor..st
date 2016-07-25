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
					self formatCommentWithStatements
						ifTrue: [ self formatStatementCommentsFor: each ].
					self space ] ].
	self newLines: self newLinesAfterTemporaries
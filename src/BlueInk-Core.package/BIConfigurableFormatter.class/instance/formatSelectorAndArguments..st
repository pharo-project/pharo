private-formatting
formatSelectorAndArguments: aMessageNode
	| newLineBetweenArguments |
	newLineBetweenArguments := self isMultiLineMessage: aMessageNode.
	self
		indent:
			(newLineBetweenArguments
				ifTrue: [ self indentsForKeywords ]
				ifFalse: [ 0 ])
		around: [ 
			self
				formatSelectorAndArguments: aMessageNode
				firstSeparator:
					((newLineBetweenArguments or: [ self newLineBeforeFirstKeyword ])
						ifTrue: [ [ self newLine ] ]
						ifFalse: [ [ self space ] ])
				restSeparator:
					(newLineBetweenArguments
						ifTrue: [ [ self newLine ] ]
						ifFalse: [ [ self space ] ]) ]
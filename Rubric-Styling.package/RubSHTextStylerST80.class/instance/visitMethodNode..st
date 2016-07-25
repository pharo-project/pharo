visiting rb nodes
visitMethodNode: aMethodNode
	| link |
	aMethodNode comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	aMethodNode arguments do: [ :argument | self addStyle: #patternArg forNode: argument ].
	link := TextMethodLink selector: aMethodNode selector.
	aMethodNode selectorParts
		with: aMethodNode keywordsPositions
		do:
			[ :keyword :position | 
			self
				addStyle: #patternKeyword
				attribute: link
				from: position
				to: position + keyword size - 1 ].
	aMethodNode pragmas do: [ :each | self visitNode: each ].
	self visitNode: aMethodNode body
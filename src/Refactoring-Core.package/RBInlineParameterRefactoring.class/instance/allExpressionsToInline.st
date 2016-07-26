private
allExpressionsToInline
	| coll |
	coll := Set new.
	self model allReferencesTo: oldSelector
		do: 
			[:each | 
			| tree |
			tree := each parseTree.
			tree notNil ifTrue: [coll addAll: (self expressionsToInlineFrom: tree)]].
	^coll asOrderedCollection
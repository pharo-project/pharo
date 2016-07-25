accessing
result
	| environment |
	environment := RBParseTreeEnvironment new.
	environment matcher: self rewriteRule.
	environment label: self name.
	self changes do: [ :change |
		(change isKindOf: RBAddMethodChange)
			ifTrue: [ environment addClass: change changeClass selector: change selector ] ].
	^ environment
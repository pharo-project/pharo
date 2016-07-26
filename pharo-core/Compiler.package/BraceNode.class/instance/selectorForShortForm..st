code generation
selectorForShortForm: nElements

	nElements > 4 ifTrue: [^ nil].
	^ #(braceWithNone braceWith: braceWith:with:
			braceWith:with:with: braceWith:with:with:with:) at: nElements + 1
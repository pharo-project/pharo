private
executeSearch: searcher 
	class withAllSubclasses do: 
			[:each | 
			each selectors do: 
					[:sel | 
					| parseTree |
					methodName := sel.
					parseTree := each parseTreeFor: sel.
					parseTree notNil ifTrue: [searcher executeTree: parseTree]]]
support
convertMethod: selector for: aClass using: searchReplacer 
	"Convert the parse tree for selector using the searchReplacer. If a
	change is made then compile it into the changeBuilder."

	| parseTree |
	parseTree := aClass parseTreeFor: selector.
	parseTree isNil ifTrue: [^self].
	(searchReplacer executeTree: parseTree) 
		ifTrue: [aClass compileTree: searchReplacer tree]
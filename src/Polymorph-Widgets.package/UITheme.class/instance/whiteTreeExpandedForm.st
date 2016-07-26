label-styles
whiteTreeExpandedForm
	"Answer the form to use for an expanded tree item when a contrasting one is needed."

	^self forms 
		at: #whiteTreeExpanded 
		ifAbsent: [ | f |
			f := self treeExpandedForm deepCopy.
			f replaceColor: Color white withColor: Color transparent.
			(f colorsUsed reject: [:c | c isTransparent]) do: [:c | f replaceColor:  c withColor: c whiter whiter whiter whiter whiter whiter].
			self forms at: #whiteTreeExpanded put: f.
			self whiteTreeExpandedForm]
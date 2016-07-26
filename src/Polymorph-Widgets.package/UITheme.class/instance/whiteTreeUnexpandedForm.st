label-styles
whiteTreeUnexpandedForm
	"Answer the form to use for an unexpanded tree item when a contrasting one is needed."

	^self forms 
		at: #whiteTreeUnexpanded
		ifAbsent: [ | f |
			f := self treeUnexpandedForm deepCopy.
			f replaceColor: Color white withColor: Color transparent.
			(f colorsUsed reject: [:c | c isTransparent]) do: [:c | f replaceColor:  c withColor: c whiter whiter whiter whiter whiter whiter].
			self forms at: #whiteTreeUnexpanded put: f.
			self whiteTreeUnexpandedForm]
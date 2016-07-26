label-styles
treeExpandedForm
	"Answer the form to use for an expanded tree item."

	^self forms at: #treeExpanded ifAbsent: [Form extent: 10@9 depth: Display depth]
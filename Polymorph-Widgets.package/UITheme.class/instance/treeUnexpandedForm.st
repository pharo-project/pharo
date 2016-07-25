label-styles
treeUnexpandedForm
	"Answer the form to use for an unexpanded tree item."

	^self forms at: #treeUnexpanded ifAbsent: [Form extent: 10@9 depth: Display depth]
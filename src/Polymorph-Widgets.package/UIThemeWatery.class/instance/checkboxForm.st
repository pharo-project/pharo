fill-styles
checkboxForm
	"Answer the form to use for a normal checkbox."

	^self forms at: #checkbox ifAbsent: [Form extent: 14@14 depth: Display depth]
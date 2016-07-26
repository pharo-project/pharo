fill-styles
radioButtonForm
	"Answer the form to use for a normal radio button."

	^self forms at: #radioButton ifAbsent: [Form extent: 14@14 depth: Display depth]
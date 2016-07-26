morph-common
privateMoveBy: delta 
	"Private! Use 'position:' instead."
	bounds := bounds translateBy: delta.
	fullBounds ifNotNil: [fullBounds := fullBounds translateBy: delta].
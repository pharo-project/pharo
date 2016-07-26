drawing
drawOn: aCanvas
	"Indicate unaccepted edits, conflicts etc."

	super drawOn: aCanvas. 
	focused ifTrue: [
		Smalltalk ui theme drawTextAdornmentFor: self color: Color orange on: aCanvas]
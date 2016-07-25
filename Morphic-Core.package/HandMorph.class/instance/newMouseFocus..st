focus handling
newMouseFocus: aMorphOrNil
	"Make the given morph the new mouse focus, canceling the previous mouse focus if any. If the argument is nil, the current mouse focus is cancelled."
	self mouseFocus: aMorphOrNil.

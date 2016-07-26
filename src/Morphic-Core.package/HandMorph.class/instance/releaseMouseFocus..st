focus handling
releaseMouseFocus: aMorph
	"If the given morph had the mouse focus before, release it"
	self mouseFocus == aMorph ifTrue:[self releaseMouseFocus].
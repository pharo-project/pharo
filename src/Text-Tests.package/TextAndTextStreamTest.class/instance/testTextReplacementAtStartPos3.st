tests
testTextReplacementAtStartPos3

	"for a Text  t,
	the following assertion should always hold:
	t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement 
	Here, the replacement text is shorteer than the text that is shall replace. "

	self should: [self replacementAtStartExample3] raise: Error
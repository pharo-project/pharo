tests
testTextReplacementAtStartPos2

  	"for a Text  t,
     the following assertion should always hold:
     t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement "

	| text1  replacement  length  |
	text1 := 'This is a simple text' copy asText.
	"without the copy, we would modify a constant that the compiler attached at the compiled method. "
	length  := 'This' size.
	replacement := 'That********' asText.
	text1 replaceFrom: 1 
        to:   length
        with: replacement
        startingAt: 1.
	self assert: text1 string size = text1 runs size.

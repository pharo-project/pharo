tests
testExampleText1
	"self run: #testExampleText1"
	"inspired by a bug report from Tim Olson.
	Text attributes are lost when the stream collection is expanded.
	Documented BUG!!!"

    | text1 text2 atts1 atts2 |
	text1 := self example1: 10. " here we will loose the attribute bold "
	text2 := self example1: 50. " here we have a larger buffer and will not loose text attributes "
	atts1 := text1 runs copyFrom: 6 to: 10. 
	atts2 := text2 runs copyFrom: 6 to: 10. 

	self assert: atts1 = atts2.
      
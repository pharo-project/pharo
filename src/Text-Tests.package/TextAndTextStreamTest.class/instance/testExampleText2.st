tests
testExampleText2
	"a Text looses its attributes when it is reversed "
	
	| text1 text2 |
	text1 := self example2.
	text2 := text1 reversed reversed.
	self assert: text1 runs = text2 runs.
 

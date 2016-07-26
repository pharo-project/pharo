tests
testPrependString
	"tests the Text>>prepend: method when prepending a String " 
	| receiver argument result expectedResult |

	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'yyyxxx' asText 
		addAttribute: TextEmphasis bold from: 4 to: 6.
	result := receiver prepend: argument.
	self assert: (result == receiver). 
	self assert: (result string = expectedResult string).
	self assert: (result runs = expectedResult runs)

	
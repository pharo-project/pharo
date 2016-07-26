tests
testAppendText
	"tests the Text>>prepend: method when appending a Text " 
	| receiver argument result expectedResult |

	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy' asText
		addAttribute: TextEmphasis italic from: 1 to: 3.		.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3;
		addAttribute: TextEmphasis italic from: 4 to: 6.
	result := receiver append: argument.
	self assert: (result == receiver). 
	self assert: (result string = expectedResult string).
	self assert: (result runs = expectedResult runs)

	
	
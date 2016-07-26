tests
testAppendString
	"tests the Text>>prepend: method when appending a String " 
	"self run: # testAppendString"

	| receiver argument result expectedResult |

	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3.
	result := receiver append: argument.
	self assert: (result == receiver). 
	self assert: (result string = expectedResult string).
	self assert: (result runs = expectedResult runs)
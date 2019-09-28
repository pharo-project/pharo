"
A TextTest is a test class for testing the behavior of Text
"
Class {
	#name : #TextTest,
	#superclass : #TestCase,
	#category : #'Text-Tests'
}

{ #category : #tests }
TextTest >> testAppend [
	| receiver argument result expectedResult |

	"Append text without attribute"
	receiver := 'xxx' asText  addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3.
	result := receiver append: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs.
	
	"Append text with attribute" 
	receiver := 'xxx' asText  addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy' asText addAttribute: TextEmphasis italic from: 1 to: 3.
	result := receiver append: argument.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3;
		addAttribute: TextEmphasis italic from: 4 to: 6.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs.
]

{ #category : #tests }
TextTest >> testPrepend [
	| receiver argument result expectedResult |

	"Prepend text without attribute"
	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'yyyxxx' asText 
		addAttribute: TextEmphasis bold from: 4 to: 6.
	result := receiver prepend: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs.

	"Prepend text with attribute"
	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy' asText 
		addAttribute: TextEmphasis italic from: 1 to: 3.
	expectedResult := 'yyyxxx' asText 
		addAttribute: TextEmphasis italic from: 1 to: 3;
		addAttribute: TextEmphasis bold from: 4 to: 6.
	result := receiver prepend: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs
	
]

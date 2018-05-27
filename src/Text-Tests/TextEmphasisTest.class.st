"
SUnit tests for TextEmphasis
"
Class {
	#name : #TextEmphasisTest,
	#superclass : #HashAndEqualsTestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextEmphasisTest >> setUp [
	super setUp.
	prototypes add: TextEmphasis bold;
		 add: TextEmphasis italic;
		 add: TextEmphasis narrow;
		 add: TextEmphasis normal;
		 add: TextEmphasis struckOut;
		 add: TextEmphasis underlined 
]

{ #category : #tests }
TextEmphasisTest >> testAdd [
	| t1 |
	t1 := TextEmphasis bold.
	t1 add: TextEmphasis italic.
	self assert: (t1 = 2r11) description: 'bold and italics didn''t add correctly'
]

{ #category : #tests }
TextEmphasisTest >> testAppendString [
	"Tests the Text>>prepend: method when appending a String" 

	| receiver argument result expectedResult |

	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3.
	result := receiver append: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs
]

{ #category : #tests }
TextEmphasisTest >> testAppendText [
	"tests the Text>>prepend: method when appending a Text " 
	| receiver argument result expectedResult |

	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy' asText
		addAttribute: TextEmphasis italic from: 1 to: 3.
	expectedResult := 'xxxyyy' asText 
		addAttribute: TextEmphasis bold from: 1 to: 3;
		addAttribute: TextEmphasis italic from: 4 to: 6.
	result := receiver append: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs
]

{ #category : #tests }
TextEmphasisTest >> testFromCode [
	| t1 |
	t1 := TextEmphasis fromCode: 2r10101.
	self assert: t1 value equals: 2r10101
]

{ #category : #tests }
TextEmphasisTest >> testPrependString [
	"Tests the Text>>prepend: method when prepending a String." 
	| receiver argument result expectedResult |

	receiver := 'xxx' asText  
		addAttribute: TextEmphasis bold from: 1 to: 3.
	argument := 'yyy'.
	expectedResult := 'yyyxxx' asText 
		addAttribute: TextEmphasis bold from: 4 to: 6.
	result := receiver prepend: argument.
	self assert: result == receiver. 
	self assert: result string equals: expectedResult string.
	self assert: result runs equals: expectedResult runs

	
]

{ #category : #tests }
TextEmphasisTest >> testPrependText [
	"Tests the Text>>prepend: method when prepending Text" 
	| receiver argument result expectedResult |

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

{ #category : #tests }
TextEmphasisTest >> testRemove [
	| t1  |
	t1 := TextEmphasis bold.
	t1 add: TextEmphasis italic.
	t1 remove: TextEmphasis bold.
	self assert: t1 = TextEmphasis italic description: 'Removing bold didn''t leave italics'
]

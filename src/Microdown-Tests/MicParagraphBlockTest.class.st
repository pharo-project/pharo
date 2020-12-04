"
I'm is a test class for testing the behavior of paragraph block.
"
Class {
	#name : #MicParagraphBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests'
}

{ #category : #tests }
MicParagraphBlockTest >> testANewLineCreatesANewParagraph [
	"**Hello 
	
	Pharo**"

	| root |
	root := parser parse: '**Hello is ... cut for real

a paragraph on two lines**'.

	self assert: root children first text equals: '**Hello is ... cut for real'.
	self assert: root children second text equals: 'a paragraph on two lines**'.
	
]

{ #category : #tests }
MicParagraphBlockTest >> testBoldMultipleLinesWithNewLine [

	| root |
	root := parser parse: '**Hello is ... cut for real
another line** and more 

a paragraph on two lines**'.

	self assert: root children first text equals: '**Hello is ... cut for real
another line** and more'.
	self assert: root children second text equals: 'a paragraph on two lines**'.
	
	self assert: (MicInlineSplitter new start: root children first text) first kind equals: #bold.
	self assert: (MicInlineSplitter new start: root children second text) first kind equals: #basic.
]

{ #category : #tests }
MicParagraphBlockTest >> testCanConsumeLine [
	"A list item does not force the creation of a new paragraph when the line is correctly indented.
	currently the implementation does not create a paragraph as a child of list item."
	
	| listitem |
	parser parse: ''. 
	listitem := parser handleLine: '- This is'.
	
	self assert: (listitem canConsumeLine: '  a paragraph in a list item').
	self deny: (listitem canConsumeLine: 'a paragraph in a list item').
	
	parser handleLine: '  a paragraph in a list item'.
	self assert: parser current equals: listitem.  
	self assert: listitem text equals: 'This is
a paragraph in a list item'.
	

]

{ #category : #tests }
MicParagraphBlockTest >> testCanConsumeLineBasic [

	| root paragraph |
	root := parser parse: ''.
	self assert: root equals: parser current.
	
	root canConsumeLine: 'This is'.
	paragraph := parser handleLine: 'This is'.
	self assert: paragraph equals: parser current.
	
	self assert: paragraph text equals: 'This is'.
	self assert: (paragraph canConsumeLine: 'Another line').
	paragraph := parser handleLine: 'Another line'.
	self assert: paragraph text equals: 'This is
Another line'.
]

{ #category : #tests }
MicParagraphBlockTest >> testCanConsumeLineOtherBlocks [

	| root paragraph |
	root := parser parse: ''.
	paragraph := parser handleLine: 'This is'.
	self assert: paragraph equals: parser current.	
	self assert: paragraph text equals: 'This is'.
	
	self deny: (paragraph canConsumeLine: '* Another line').
	self deny: (paragraph canConsumeLine: '```Another line').
	self deny: (paragraph canConsumeLine: '> Another line').
	self deny: (paragraph canConsumeLine: '').
	self deny: (paragraph canConsumeLine: '# Section1').
	self deny: (paragraph canConsumeLine: '- ').
	self deny: (paragraph canConsumeLine: '1.  ').
]

{ #category : #tests }
MicParagraphBlockTest >> testHandlingSequenceOfBasicLinesProducesAParagraph [

	| root paragraph |
	root := parser parse: ''.
	paragraph := parser handleLine: 'This is'.
	paragraph := parser handleLine: 'Another line'.
	paragraph := parser handleLine: '  And that other line'.
	self assert: paragraph text equals: 'This is
Another line
And that other line'.
	self assert: root children size equals: 1
]

{ #category : #tests }
MicParagraphBlockTest >> testSingleParagraph [
	| root |
	root := parser parse: 'This is
a paragraph on two lines'.
	self 
		assert: root children first contents 
		equals: '[ This is
a paragraph on two lines ]'

]

{ #category : #tests }
MicParagraphBlockTest >> testSingleParagraphWithIndentedText [
	| source root expected |
	source := 'This is
   a paragraph with spaces
	and one with tabs'.
	expected := 'This is
a paragraph with spaces
and one with tabs'.
	root := parser parse: source.
	self assert: root children first text equals: expected
]

{ #category : #tests }
MicParagraphBlockTest >> testTwoSingleParagraphs [
	| root |
	root := parser parse: 'This is

a paragraph on two lines'.
	self assert: root children first text equals: 'This is'.
	self assert: root children second text equals: 'a paragraph on two lines'.
	
]

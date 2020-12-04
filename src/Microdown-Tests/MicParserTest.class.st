Class {
	#name : #MicParserTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests'
}

{ #category : #tests }
MicParserTest >> testEmpty [
	| source root |
	source := ''.
	root := parser parse: source.
	self assert: root children size equals: 0.

]

{ #category : #tests }
MicParserTest >> testEmptyOnMultipleLines [
	| source root |
	source := '


'.
	root := parser parse: source.
	self assert: root children size equals: 0

]

{ #category : #tests }
MicParserTest >> testIsAListDash [

	self assert: (parser isAList: '- one')
	
]

{ #category : #tests }
MicParserTest >> testIsAListNumbered [

	self assert: (parser isAList: '11. one')
	
]

{ #category : #tests }
MicParserTest >> testIsAListPlus [

	self assert: (parser isAList: '+ one')
	
]

{ #category : #tests }
MicParserTest >> testIsAListStar [

	self assert: (parser isAList: '* one')
	
]

{ #category : #tests }
MicParserTest >> testIsAListStar2 [

	self assert: (parser isAList: '  * one' trim)
	
]

{ #category : #tests }
MicParserTest >> testOnceParsedCurrentShouldBeTheRootblock [

	| source root |
	source := 'This is
a paragraph on two lines'.
	root := parser parse: source.
	self assert: parser current == root
]

{ #category : #tests }
MicParserTest >> testParsingLogic [
	| source root textBody argument line code |
	
	"ready for code factory"
	textBody := 'my fancy code
is really cool'.
	argument := 'ArgumentGoesHere'.
	source := (parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup) readStream. 
	
	line := source nextLine.
	"First we get the root block"
	root := parser parse: ''.
	self assert: parser current equals: root.
	self assert: (root canConsumeLine: line).
	code := parser handleLine: line.
	self assert: code parent equals: root.
	self assert: code equals: parser current. 
	
	"now new line"
	parser handleLine: source nextLine.
	self deny:  code isClosed.
	parser handleLine: source nextLine.
	self deny:  code isClosed.
	self assert: code equals: parser current.
	parser handleLine: source nextLine.
	self assert: code isClosed. 
	parser handleLine: 'another line'.
	self deny: code equals: parser current. 
	
	
	
	
]

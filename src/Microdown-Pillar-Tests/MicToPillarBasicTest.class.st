Class {
	#name : #MicToPillarBasicTest,
	#superclass : #TestCase,
	#instVars : [
		'pillarDocument'
	],
	#category : #'Microdown-Pillar-Tests'
}

{ #category : #tests }
MicToPillarBasicTest >> parserClass [
	^ MicroDownParser
]

{ #category : #tests }
MicToPillarBasicTest >> testCodeBlock [

	| document |
	document := self parserClass new parse: '```language=pharo
	1-2 
```'.
	self assert: document asPillar children first class equals: PRCodeblock.
	
]

{ #category : #tests }
MicToPillarBasicTest >> testCodeInText [

	| document pillarDocument |
	document := self parserClass new parse: 'The method `foo` is fun'.
	pillarDocument := document asPillar.
	self assert: pillarDocument children first class equals: PRParagraph.
	self assert: pillarDocument children first children second class equals: PRMonospaceFormat 
	
	
]

{ #category : #tests }
MicToPillarBasicTest >> testHeader [ 

	| document |
	document := self parserClass new parse: '# Header1
	
## Header 2
	
### Header 3'.
	pillarDocument := document asPillar.
	"we could have use Grammar headerMarkup to build the text but this is clearer like that"
	self assert: pillarDocument children first class equals: PRHeader.
	self assert: pillarDocument children third class equals: PRHeader.
	self assert: pillarDocument children fifth class equals: PRHeader
	
]

{ #category : #tests }
MicToPillarBasicTest >> testNumberedList [
	| document |
	document := (self parserClass new 
		parse: MicMicroDownSnippetFactory new orderedListWithTwoItemsSample) asPillar.
	self assert: document children first class equals: PROrderedList.
	self
		assert: document children first children first class
		equals: PRListItem.
	self
		assert: document children first children second class
		equals: PRListItem
]

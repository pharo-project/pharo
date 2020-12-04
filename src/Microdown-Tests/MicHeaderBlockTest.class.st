"
I'm a test class for testing the behavior of MicHeaderBlock
"
Class {
	#name : #MicHeaderBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests'
}

{ #category : #tests }
MicHeaderBlockTest >> testCanConsumeLine [

	| root headerString |
	headerString := (parser headerMarkup, ' Level1').
	root := parser parse: headerString.
	self deny: (root children first canConsumeLine: headerString) 
]

{ #category : #tests }
MicHeaderBlockTest >> testContents [
	| root |
	root := parser parse: (parser headerMarkup, ' Level1').
	self assert: root children first contents equals: '[ 1/Level1 ]'
]

{ #category : #tests }
MicHeaderBlockTest >> testHeaderLevel1 [
	| root |
	root := parser parse: (parser headerMarkup, ' Level1').
	self assert: root children size equals: 1.
	self assert: root children first header equals: 'Level1'.
	self assert: root children first level equals: 1
]

{ #category : #tests }
MicHeaderBlockTest >> testHeaderLevel1NoSpace [
	| source root |
	source := parser headerMarkup, 'Level1'.
	root := parser parse: source.
	self assert: root children size equals: 1.
	self assert: root children first header equals: 'Level1'.
	self assert: root children first level equals: 1
]

{ #category : #tests }
MicHeaderBlockTest >> testHeaderLevel4 [
	| source root |
	source := (parser headerMarkup repeat: 4), '    Level four'.
	root := parser parse: source.
	self assert: root children size equals: 1.
	self assert: root children first header equals: 'Level four'.
	self assert: root children first level equals: 4
]

{ #category : #tests }
MicHeaderBlockTest >> testHeaderLevel6 [
	| source root |
	source := (parser headerMarkup repeat: 6), '    Level six'.
	root := parser parse: source.
	self assert: root children size equals: 1.
	self assert: root children first header equals: 'Level six'.
	self assert: root children first level equals: 6
]

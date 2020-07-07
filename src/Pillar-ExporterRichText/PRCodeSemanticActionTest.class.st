Class {
	#name : #PRCodeSemanticActionTest,
	#superclass : #TestCase,
	#instVars : [
		'sample'
	],
	#category : #'Pillar-ExporterRichText-Tests'
}

{ #category : #'test - bogus entry' }
PRCodeSemanticActionTest >> testDoubleDollars [

	self deny: (PRSemanticAction from: '$$') hasEntity 
]

{ #category : #'test - bogus entry' }
PRCodeSemanticActionTest >> testEmptyString [

	self deny: (PRSemanticAction from: '') hasEntity 
]

{ #category : #'test - bogus entry' }
PRCodeSemanticActionTest >> testMoreSpaces [

	self deny: (PRSemanticAction from: '    ') hasEntity 
]

{ #category : #'test - bogus entry' }
PRCodeSemanticActionTest >> testOneSpace [

	self deny: (PRSemanticAction from: ' ') hasEntity 
]

{ #category : #'tests - metaclass' }
PRCodeSemanticActionTest >> testReferenceToABrokenMetaClass [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point2 class')) children first text.
	self assert: (PRSemanticAction from: text) entity isNil
]

{ #category : #'tests - metaclass' }
PRCodeSemanticActionTest >> testReferenceToABrokenMetaClass2 [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point grgrgr')) children first text.
	self assert: (PRSemanticAction from: text) entity isNil
]

{ #category : #'tests - class' }
PRCodeSemanticActionTest >> testReferenceToAClass [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point')) children first text.
	self assert: (PRSemanticAction from: text) entity equals: Point
]

{ #category : #'tests - metaclass' }
PRCodeSemanticActionTest >> testReferenceToAMetaClass [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point class')) children first text.
	self assert: (PRSemanticAction from: text) entity equals: Point class
]

{ #category : #'tests - methods' }
PRCodeSemanticActionTest >> testReferenceToAMethod [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point >> #setX:setY:')) children first text.
	self assert: (PRSemanticAction from: text) entity name equals: 'Point>>#setX:setY:'
]

{ #category : #'tests - methods' }
PRCodeSemanticActionTest >> testReferenceToANonExistingMethod [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point >> 12+3 ')) children first text.
	self assert: (PRSemanticAction from: text) entity isNil
]

{ #category : #'tests - methods' }
PRCodeSemanticActionTest >> testReferenceToANonExistingMethodBecauseOfBogusClass [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point222 >> #setX:setY:')) children first text.
	self assert: (PRSemanticAction from: text) entity isNil
]

{ #category : #'tests - methods' }
PRCodeSemanticActionTest >> testReferenceToANonExistingMethodBecauseOfNoSymbol [

	| text |
	self skip.
	"for now scantoken convert 'setX:setY: into #setX:setY:'
	so I cannot distinguish"
	
	text := (PRMonospaceFormat with: (PRText content: 'Point >> setX:setY:')) children first text.
	self assert: (PRSemanticAction from: text) entity isNil
]

{ #category : #'tests - package' }
PRCodeSemanticActionTest >> testReferenceToANonExistingPackage [

	| text |
	text := (PRMonospaceFormat with: (PRText content: #'Pillar')) children first text.
	self assert: (PRSemanticAction from: text) entity isNil
]

{ #category : #'tests - package' }
PRCodeSemanticActionTest >> testReferenceToAPackage [

	| text |
	text := (PRMonospaceFormat with: (PRText content: #'''Pillar-ExporterRichText''')) children first text.
	self assert: (PRSemanticAction from: text) entity name equals: 'Pillar-ExporterRichText'
]

{ #category : #'tests - class' }
PRCodeSemanticActionTest >> testReferenceToNonExistingClass [

	| text |
	text := (PRMonospaceFormat with: (PRText content: 'Point22')) children first text.
	self deny: (PRSemanticAction from: text) hasEntity
]

{ #category : #'test - bogus entry' }
PRCodeSemanticActionTest >> testTwoSpaces [

	self deny: (PRSemanticAction from: '  ') hasEntity 
]

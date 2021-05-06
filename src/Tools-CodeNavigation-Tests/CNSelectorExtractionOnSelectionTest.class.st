Class {
	#name : #CNSelectorExtractionOnSelectionTest,
	#superclass : #TestCase,
	#instVars : [
		'sourceCode'
	],
	#category : #'Tools-CodeNavigation-Tests'
}

{ #category : #helpers }
CNSelectorExtractionOnSelectionTest >> select: someText [ 
	
	sourceCode := someText.
]

{ #category : #helpers }
CNSelectorExtractionOnSelectionTest >> selectedSelector [

	^ CNSelectorExtractor new extractSelectorFromSelection: sourceCode
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectCascadeFirstMessageShouldMatchFirstMessageSelector [

	self select: 'foo bar: 7; fum'.
	
	self assert: self selectedSelector equals: #bar:
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectCascadeFirstMessageWithoutReceiverShouldMatchFirstMessageSelector [

	self select: 'bar: 7; fum'.

	self assert: self selectedSelector equals: #bar:
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectInvalidCode [

	self select: 'even : 2'.
	
	self assert: self selectedSelector equals: #even
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectInvalidCodeWithPossibleSelector [
	"Code has faulty code"
	self select: 'anObject do : 1 with: 2'.
	self assert: self selectedSelector equals: #do
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectKeywordMessageShouldMatchKeywordSelector [

	self select: 'a foo: 17 bar: (some message: #toto) fum: [ :will | it break ]'.
	
	self assert: self selectedSelector equals: #foo:bar:fum:
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectKeywordMessageWithBrokenLastArgumentShouldMatchKeywordSelector [

	self select: 'a foo: 17 bar: (some message: #toto) fum: [ :will | it b'.
	
	self assert: self selectedSelector equals: #foo:bar:fum:
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectKeywordMessageWithBrokenMiddleArgumentShouldMatchKeywordSelector [

	self select: 'a foo: 17 bar: (some message: # fum: [ :will | it break ]'.
	
	self assert: self selectedSelector equals: #foo:bar:
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectKeywordMessageWithoutReceiverAndComplexArgumentsShouldMatchKeywordSelector [

	self select: 'foo: 17 bar: (some message: #toto) fum: [ :will | it break ]'.

	self assert: self selectedSelector equals: #foo:bar:fum:
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectKeywordMessageWithoutReceiverShouldMatchKeywordSelector [

	self select: 'method: a zkx: bsq pn: c'.

	self assert: self selectedSelector equals: #method:zkx:pn:
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectMultipleStatementsShouldMatchFirstStatement [

	self select: 'foo. second statement'.
	
	self assert: self selectedSelector equals: #foo
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectReturnShouldMatchToken [

	self select: '^ foo'.
	
	self assert: self selectedSelector equals: #foo
]

{ #category : #'tests - selection' }
CNSelectorExtractionOnSelectionTest >> testSelectTokenShouldMatchToken [

	self select: 'foo'.
	
	self assert: self selectedSelector equals: #foo
]

"
A FTColumnTest is a test class for testing the behavior of FTColumn
"
Class {
	#name : #FTColumnTest,
	#superclass : #TestCase,
	#instVars : [
		'column'
	],
	#category : #'Morphic-Widgets-FastTable-Tests-Model'
}

{ #category : #accessing }
FTColumnTest >> actualClass [
	^ FTColumn
]

{ #category : #running }
FTColumnTest >> setUp [
	super setUp.
	column := self actualClass new
]

{ #category : #tests }
FTColumnTest >> testColumnHasIconWithSorting [
	column sortableOnProperty: #name.
	self assert: column sortingIcon isNotNil
]

{ #category : #tests }
FTColumnTest >> testDisableSort [
	column disableSort.
	self deny: column isSortEnabled
]

{ #category : #tests }
FTColumnTest >> testIsSortEnabled [
	column disableSort.
	self deny: column isSortEnabled.
	column sortableOnProperty: #name.
	self assert: column isSortEnabled
]

{ #category : #tests }
FTColumnTest >> testNoIconWithoutSorting [
	column disableSort.
	self assert: column sortingIcon isNil
]

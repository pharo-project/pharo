Class {
	#name : #MorphicColumnedListCommonPropertiestTest,
	#superclass : #MorphicAbstractListCommonPropertiestTest,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #running }
MorphicColumnedListCommonPropertiestTest >> classToTest [
	^ ColumnedListPresenter
]

{ #category : #running }
MorphicColumnedListCommonPropertiestTest >> initializeTestedInstance [

	super initializeTestedInstance.
	presenter addColumn: (StringTableColumn title: 'Number' evaluated: #yourself).
	
	"Hide headers. We will try to click the first element, and the header is an extra element."
	presenter hideColumnHeaders.
]

{ #category : #tests }
MorphicColumnedListCommonPropertiestTest >> testAddColumnInPresenterAddsItToWidget [

	| newColumn |
	newColumn := StringTableColumn title: 'Hex' evaluated: #hex.
	self presenter
		addColumn: newColumn.

	self assert: self widget columns last model equals: newColumn
]

{ #category : #tests }
MorphicColumnedListCommonPropertiestTest >> testHideColumnHeadersInPresenterHidesThemInWidget [

	self presenter hideColumnHeaders.
	
	self assert: self widget container headerRow isNil
]

{ #category : #tests }
MorphicColumnedListCommonPropertiestTest >> testListWithoutColumnsHasSingleColumn [

	self assert: self widget columns size equals: 1
]

{ #category : #tests }
MorphicColumnedListCommonPropertiestTest >> testShowColumnHeadersInPresenterShowsThemInWidget [

	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].

	self presenter showColumnHeaders.
	
	self assert: self widget container headerRow notNil
]

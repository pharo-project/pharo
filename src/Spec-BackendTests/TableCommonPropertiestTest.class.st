Class {
	#name : #TableCommonPropertiestTest,
	#superclass : #AbstractListCommonPropertiestTest,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
TableCommonPropertiestTest >> classToTest [
	^ TablePresenter
]

{ #category : #running }
TableCommonPropertiestTest >> initializeTestedInstance [

	super initializeTestedInstance.
	presenter addColumn: (StringTableColumn title: 'Number' evaluated: #yourself).
	
	"Hide headers. We will try to click the first element, and the header is an extra element."
	presenter hideColumnHeaders.
]

{ #category : #tests }
TableCommonPropertiestTest >> testAddColumnInPresenterAddsItToWidget [

	| newColumn |
	newColumn := StringTableColumn title: 'Hex' evaluated: #hex.
	self presenter
		addColumn: newColumn.

	self assert: self widget columns last id equals: newColumn title
]

{ #category : #tests }
TableCommonPropertiestTest >> testHideColumnHeadersInPresenterHidesThemInWidget [

	self presenter hideColumnHeaders.
	
	self deny: self adapter isShowColumnHeaders  
]

{ #category : #tests }
TableCommonPropertiestTest >> testListWithoutColumnsHasSingleColumn [

	self assert: self widget columns size equals: 1
]

{ #category : #tests }
TableCommonPropertiestTest >> testShowColumnHeadersInPresenterShowsThemInWidget [

	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].

	self presenter showColumnHeaders.
	
	self assert: self adapter isShowColumnHeaders
]

Class {
	#name : #SpTableCommonPropertiestTest,
	#superclass : #SpAbstractListCommonPropertiestTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpTableCommonPropertiestTest >> classToTest [
	^ SpTablePresenter
]

{ #category : #running }
SpTableCommonPropertiestTest >> initializeTestedInstance [

	super initializeTestedInstance.
	presenter addColumn: (SpStringTableColumn title: 'Number' evaluated: #yourself).
	
	"Hide headers. We will try to click the first element, and the header is an extra element."
	presenter hideColumnHeaders.
]

{ #category : #tests }
SpTableCommonPropertiestTest >> testAddColumnInPresenterAddsItToWidget [

	| newColumn |
	newColumn := SpStringTableColumn title: 'Hex' evaluated: #hex.
	self presenter
		addColumn: newColumn.

	self assert: self adapter columns last id equals: newColumn title
]

{ #category : #tests }
SpTableCommonPropertiestTest >> testHideColumnHeadersInPresenterHidesThemInWidget [

	self presenter hideColumnHeaders.
	
	self deny: self adapter isShowColumnHeaders  
]

{ #category : #tests }
SpTableCommonPropertiestTest >> testListWithoutColumnsHasSingleColumn [

	self assert: self adapter columns size equals: 1
]

{ #category : #tests }
SpTableCommonPropertiestTest >> testShowColumnHeadersInPresenterShowsThemInWidget [

	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].

	self presenter showColumnHeaders.
	
	self assert: self adapter isShowColumnHeaders
]

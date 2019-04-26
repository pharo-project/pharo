Class {
	#name : #ListCommonPropertiestTest,
	#superclass : #AbstractListCommonPropertiestTest,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
ListCommonPropertiestTest >> classToTest [
	^ ListPresenter
]

{ #category : #running }
ListCommonPropertiestTest >> testRemoveHeaderTitleInPresenterRemovesColumnHeaderMorph [

	| tableContainer |
	
	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].
	
	self presenter headerTitle: 'test'.
	self presenter hideHeaderTitle.

	backendForTest denyHasHeader: self widget.
]

{ #category : #running }
ListCommonPropertiestTest >> testSetColumnTitleInPresenterPutsColumnHeaderMorph [

	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].

	self presenter headerTitle: 'test'.

	backendForTest assertListHeaderOf: self widget equals: #('test')
]

{ #category : #running }
ListCommonPropertiestTest >> testSetColumnTitleInPresenterShowsThemInWidget [

	self presenter headerTitle: 'test'.
	
	self assert: self adapter isShowColumnHeaders
]

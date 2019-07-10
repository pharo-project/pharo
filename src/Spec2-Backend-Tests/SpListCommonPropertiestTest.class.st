Class {
	#name : #SpListCommonPropertiestTest,
	#superclass : #SpAbstractListCommonPropertiestTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpListCommonPropertiestTest >> classToTest [
	^ SpListPresenter
]

{ #category : #running }
SpListCommonPropertiestTest >> testChangingFromMultipleToSingleSelection [
	presenter beMultipleSelection.
	self assert: presenter isMultipleSelection.
	presenter beSingleSelection.
	self deny: presenter isMultipleSelection
]

{ #category : #running }
SpListCommonPropertiestTest >> testChangingFromSingleToMultipleSelection [
	presenter beSingleSelection.
	self deny: presenter isMultipleSelection.
	presenter beMultipleSelection.
	self assert: presenter isMultipleSelection
]

{ #category : #running }
SpListCommonPropertiestTest >> testEnablingFilteringUpdateOpenedList [
	self deny: self adapter hasFilter.
	presenter enableItemSubstringFilter.
	self assert: self adapter hasFilter.
	presenter itemFilter: nil.
	self deny: self adapter hasFilter
]

{ #category : #running }
SpListCommonPropertiestTest >> testRemoveHeaderTitleInPresenterRemovesColumnHeaderMorph [
	SystemVersion current major = 7 ifTrue: [ "Test failing in Pharo7 due to a bug in FastTable" ^ self skip ].

	self presenter headerTitle: 'test'.
	self presenter hideHeaderTitle.

	backendForTest denyHasHeader: self widget
]

{ #category : #running }
SpListCommonPropertiestTest >> testSetColumnTitleInPresenterPutsColumnHeaderMorph [

	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].

	self presenter headerTitle: 'test'.

	backendForTest assertListHeaderOf: self widget equals: #('test')
]

{ #category : #running }
SpListCommonPropertiestTest >> testSetColumnTitleInPresenterShowsThemInWidget [

	self presenter headerTitle: 'test'.
	
	self assert: self adapter isShowColumnHeaders
]

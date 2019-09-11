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
ListCommonPropertiestTest >> testChangingFromMultipleToSingleSelection [
	presenter beMultipleSelection.
	self assert: presenter isMultipleSelection.
	presenter beSingleSelection.
	self deny: presenter isMultipleSelection
]

{ #category : #running }
ListCommonPropertiestTest >> testChangingFromSingleToMultipleSelection [
	presenter beSingleSelection.
	self deny: presenter isMultipleSelection.
	presenter beMultipleSelection.
	self assert: presenter isMultipleSelection
]

{ #category : #running }
ListCommonPropertiestTest >> testEnablingFilteringUpdateOpenedList [
	self deny: self adapter hasFilter.
	presenter enableItemSubstringFilter.
	self assert: self adapter hasFilter.
	presenter itemFilterBlock: nil.
	self deny: self adapter hasFilter
]

{ #category : #running }
ListCommonPropertiestTest >> testRemoveHeaderTitleInPresenterRemovesColumnHeaderMorph [
	SystemVersion current major = 7 ifTrue: [ "Test failing in Pharo7 due to a bug in FastTable" ^ self skip ].

	self presenter headerTitle: 'test'.
	self presenter hideHeaderTitle.

	backendForTest denyHasHeader: self widget
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

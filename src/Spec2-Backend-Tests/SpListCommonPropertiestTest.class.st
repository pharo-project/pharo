Class {
	#name : #SpListCommonPropertiestTest,
	#superclass : #SpAbstractListCommonPropertiestTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpListCommonPropertiestTest >> classToTest [
	^ SpListPresenter
]

{ #category : #tests }
SpListCommonPropertiestTest >> testChangingFromMultipleToSingleSelection [
	presenter beMultipleSelection.
	self assert: presenter isMultipleSelection.
	presenter beSingleSelection.
	self deny: presenter isMultipleSelection
]

{ #category : #tests }
SpListCommonPropertiestTest >> testChangingFromSingleToMultipleSelection [
	presenter beSingleSelection.
	self deny: presenter isMultipleSelection.
	presenter beMultipleSelection.
	self assert: presenter isMultipleSelection
]

{ #category : #tests }
SpListCommonPropertiestTest >> testEnablingFilteringUpdateOpenedList [
	self deny: self adapter hasFilter.
	presenter enableItemSubstringFilter.
	self assert: self adapter hasFilter.
	presenter itemFilter: nil.
	self deny: self adapter hasFilter
]

{ #category : #tests }
SpListCommonPropertiestTest >> testRemoveHeaderTitleInPresenterRemovesColumnHeaderMorph [
	self flag: #pharo7.
	SystemVersion current major < 8 ifTrue: [ self skip. "Feature not supported in Pharo 7" ].

	self presenter headerTitle: 'test'.
	self presenter hideHeaderTitle.

	backendForTest denyHasHeader: self adapter
]

{ #category : #'tests - scrolling' }
SpListCommonPropertiestTest >> testScrollDownToNonExistingIndexScrollsBottomToLastIndex [
	self flag: #pharo7.
	SystemVersion current major < 8 ifTrue: [ self skip. "Feature not supported in Pharo 7" ].
	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 600.

	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.

	self assert: presenter verticalAlignment lastVisibleRowIndex equals: 500
]

{ #category : #'tests - scrolling' }
SpListCommonPropertiestTest >> testScrollDownToPosteriorIndexScrollsBottomToIndex [
	self flag: #pharo7.
	SystemVersion current major < 8 ifTrue: [ self skip. "Feature not supported in Pharo 7" ].
	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 100.

	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.

	"Some backends will show entirely row 100 and a part of the next row, so row 101 is also a valid answer"
	self assert: (presenter verticalAlignment lastVisibleRowIndex between: 100 and: 101)
]

{ #category : #'tests - scrolling' }
SpListCommonPropertiestTest >> testScrollUpToNegativeIndexScrollsTopToFirstIndex [
	self flag: #pharo7.
	SystemVersion current major < 8 ifTrue: [ self skip. "Feature not supported in Pharo 7" ].
	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 100.
	self openInstance.

	presenter verticalAlignment desiredVisibleRow: -1.

	self assert: presenter verticalAlignment firstVisibleRowIndex equals: 1
]

{ #category : #'tests - scrolling' }
SpListCommonPropertiestTest >> testScrollUpToPreviousIndexScrollsTopToIndex [
	self flag: #pharo7.
	SystemVersion current major < 8 ifTrue: [ self skip. "Feature not supported in Pharo 7" ].
	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 100.
	self openInstance.

	presenter verticalAlignment desiredVisibleRow: 50.

	self assert: presenter verticalAlignment firstVisibleRowIndex equals: 50
]

{ #category : #tests }
SpListCommonPropertiestTest >> testSetColumnTitleInPresenterPutsColumnHeaderMorph [

	SystemVersion current major = 7
		ifTrue: [
			"Test failing in Pharo7 due to a bug in FastTable"
			^ self skip ].

	self presenter headerTitle: 'test'.

	backendForTest assertListHeaderOf: self adapter equals: #('test')
]

{ #category : #tests }
SpListCommonPropertiestTest >> testSetColumnTitleInPresenterShowsThemInWidget [

	self presenter headerTitle: 'test'.
	
	self assert: self adapter isShowColumnHeaders
]

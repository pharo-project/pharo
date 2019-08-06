Class {
	#name : #SpAbstractListCommonPropertiestTest,
	#superclass : #SpAbstractWidgetAdapterTestCase,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #testing }
SpAbstractListCommonPropertiestTest class >> isAbstract [
	^ self == SpAbstractListCommonPropertiestTest
]

{ #category : #running }
SpAbstractListCommonPropertiestTest >> initializeTestedInstance [ 

	presenter items: #(10 20 30).
]

{ #category : #running }
SpAbstractListCommonPropertiestTest >> testChangeListInPresenterUpdatesWidget [

	self presenter items: #( '1' '2' '3').

	backendForTest assertList: self widget displayItems: #('1' '2' '3')
]

{ #category : #running }
SpAbstractListCommonPropertiestTest >> testDoubleClickActivatesRowInDoubleClickActivationMode [

	| activated |
	activated := false.
	self presenter activateOnDoubleClick.
	self presenter whenActivatedDo: [ activated := true ].
	
	backendForTest doubleClickFirstRowAndColumn: self widget.
		
	self assert: activated
]

{ #category : #'tests-scrolling' }
SpAbstractListCommonPropertiestTest >> testFirstVisibleRowIsInitiallyFirstRow [

	presenter items: (1 to: 500).
	self openInstance.
	self assert: presenter verticalAlignment firstVisibleRowIndex equals: 1.
]

{ #category : #tests }
SpAbstractListCommonPropertiestTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpMockMenuPresenter new).
	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]

{ #category : #'tests-scrolling' }
SpAbstractListCommonPropertiestTest >> testScrollDownToNonExistingIndexScrollsBottomToLastIndex [

	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 600.

	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.

	self assert: presenter verticalAlignment lastVisibleRowIndex equals: 500
]

{ #category : #'tests-scrolling' }
SpAbstractListCommonPropertiestTest >> testScrollDownToPosteriorIndexScrollsBottomToIndex [

	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 100.

	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.
	
	"Some backends will show entirely row 100 and a part of the next row, so row 101 is also a valid answer"
	self assert: (presenter verticalAlignment lastVisibleRowIndex between: 100 and: 101).
]

{ #category : #'tests-scrolling' }
SpAbstractListCommonPropertiestTest >> testScrollUpToNegativeIndexScrollsTopToFirstIndex [

	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 100.
	self openInstance.

	presenter verticalAlignment desiredVisibleRow: -1.
	
	self assert: presenter verticalAlignment firstVisibleRowIndex equals: 1.
]

{ #category : #'tests-scrolling' }
SpAbstractListCommonPropertiestTest >> testScrollUpToPreviousIndexScrollsTopToIndex [

	presenter items: (1 to: 500).
	presenter verticalAlignment desiredVisibleRow: 100.
	self openInstance.

	presenter verticalAlignment desiredVisibleRow: 50.
	
	self assert: presenter verticalAlignment firstVisibleRowIndex equals: 50.
]

{ #category : #running }
SpAbstractListCommonPropertiestTest >> testSingleClickActivatesRowInSingleClickActivationMode [

	| activated |
	activated := false.
	self presenter
		activateOnSingleClick;
		whenActivatedDo: [ activated := true ].

	backendForTest clickFirstRowAndColumn: self widget.
	
	self assert: activated.
]

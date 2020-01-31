Class {
	#name : #SpAbstractListCommonPropertiestTest,
	#superclass : #SpAbstractWidgetAdapterTest,
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

	backendForTest assertList: self adapter displayItems: #('1' '2' '3')
]

{ #category : #running }
SpAbstractListCommonPropertiestTest >> testDoubleClickActivatesRowInDoubleClickActivationMode [

	| activated |
	activated := false.
	self presenter activateOnDoubleClick.
	self presenter whenActivatedDo: [ activated := true ].
	
	backendForTest doubleClickFirstRowAndColumn: self adapter.
		
	self assert: activated
]

{ #category : #'tests - scrolling' }
SpAbstractListCommonPropertiestTest >> testFirstVisibleRowIsInitiallyFirstRow [
	presenter items: (1 to: 500).
	self openInstance.
	self assert: presenter verticalAlignment firstVisibleRowIndex equals: 1
]

{ #category : #tests }
SpAbstractListCommonPropertiestTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpMockMenuPresenter new).
	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]

{ #category : #running }
SpAbstractListCommonPropertiestTest >> testSingleClickActivatesRowInSingleClickActivationMode [

	| activated |
	activated := false.
	self presenter
		activateOnSingleClick;
		whenActivatedDo: [ activated := true ].

	backendForTest clickFirstRowAndColumn: self adapter.
	
	self assert: activated.
]

{ #category : #tests }
SpAbstractListCommonPropertiestTest >> testTypePerformsSearch [

	self presenter selection selectIndex: 1. "10"
	self adapter type: '2'. 
	self 
		assert: self presenter selection selectedIndex 
		equals: 2 "20"
]

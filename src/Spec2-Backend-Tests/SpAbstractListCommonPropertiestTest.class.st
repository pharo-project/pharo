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

	backendForTest clickFirstRowAndColumn: self widget.
	
	self assert: activated.
]

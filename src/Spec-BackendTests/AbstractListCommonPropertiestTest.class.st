Class {
	#name : #AbstractListCommonPropertiestTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #testing }
AbstractListCommonPropertiestTest class >> isAbstract [
	^ self == AbstractListCommonPropertiestTest
]

{ #category : #running }
AbstractListCommonPropertiestTest >> initializeTestedInstance [ 

	presenter items: #(10 20 30).
]

{ #category : #running }
AbstractListCommonPropertiestTest >> testChangeListInPresenterUpdatesWidget [

	self presenter items: #( '1' '2' '3').

	backendForTest assertList: self widget displayItems: #('1' '2' '3')
]

{ #category : #running }
AbstractListCommonPropertiestTest >> testDoubleClickActivatesRowInDoubleClickActivationMode [

	| activated |
	activated := false.
	self presenter activateOnDoubleClick.
	self presenter whenActivatedDo: [ activated := true ].
	
	backendForTest doubleClickFirstRowAndColumn: self widget.
		
	self assert: activated
]

{ #category : #running }
AbstractListCommonPropertiestTest >> testSingleClickActivatesRowInSingleClickActivationMode [

	| activated |
	activated := false.
	self presenter
		activateOnSingleClick;
		whenActivatedDo: [ activated := true ].

	backendForTest clickFirstRowAndColumn: self widget.
	
	self assert: activated.
]

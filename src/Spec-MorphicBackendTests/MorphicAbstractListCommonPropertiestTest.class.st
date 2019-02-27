Class {
	#name : #MorphicAbstractListCommonPropertiestTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #testing }
MorphicAbstractListCommonPropertiestTest class >> isAbstract [
	^ self == MorphicAbstractListCommonPropertiestTest
]

{ #category : #running }
MorphicAbstractListCommonPropertiestTest >> initializeTestedInstance [ 

	presenter items: #(10 20 30).
]

{ #category : #running }
MorphicAbstractListCommonPropertiestTest >> testChangeListInPresenterUpdatesWidget [

	| tableContainer tableRowContents |
	self presenter items: #( '1' '2' '3').

	tableContainer := self widget submorphs first.
	tableRowContents := tableContainer submorphs collect: [ :row | 
		| cell |
		cell := row submorphs first.
		cell submorphs first contents ].
	
	self assert: tableRowContents first equals: '1'.
	self assert: tableRowContents second equals: '2'.
	self assert: tableRowContents third equals: '3'.
]

{ #category : #running }
MorphicAbstractListCommonPropertiestTest >> testDoubleClickActivatesRowInDoubleClickActivationMode [

	| activated |
	activated := false.
	self presenter activateOnDoubleClick.
	self presenter whenActivatedDo: [ activated := true ].
	
	self widget doubleClick: (MouseEvent new
		setPosition: self widget submorphs first submorphs first bounds center;
		yourself).
	
	self assert: activated
]

{ #category : #running }
MorphicAbstractListCommonPropertiestTest >> testSingleClickActivatesRowInSingleClickActivationMode [

	| activated |
	activated := false.
	self presenter
		activateOnSingleClick;
		whenActivatedDo: [ activated := true ].
	World doOneCycle.
	
	self widget mouseDown: (MouseEvent new
		setHand: HandMorph new;
		setPosition: self widget submorphs first submorphs first bounds center;
		yourself).
	
	self assert: activated.
]

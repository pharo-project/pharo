Class {
	#name : #SpAbstractSelectionModeTest,
	#superclass : #TestCase,
	#instVars : [
		'presenter'
	],
	#category : #'Spec2-Tests-Layout'
}

{ #category : #testing }
SpAbstractSelectionModeTest class >> isAbstract [
	^ self = SpAbstractSelectionModeTest
]

{ #category : #testing }
SpAbstractSelectionModeTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #running }
SpAbstractSelectionModeTest >> setUp [
	super setUp.
	presenter := SpListPresenter new.
	presenter items: (1 to: 10)
]

{ #category : #tests }
SpAbstractSelectionModeTest >> testSubscriptionsAreTransfered [
	self subclassResponsibility
]

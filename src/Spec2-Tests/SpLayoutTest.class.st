Class {
	#name : #SpLayoutTest,
	#superclass : #SpSpecTest,
	#instVars : [
		'layout'
	],
	#category : #'Spec2-Tests-Layout'
}

{ #category : #testing }
SpLayoutTest class >> isAbstract [

	^ self == SpLayoutTest
]

{ #category : #running }
SpLayoutTest >> classToTest [

	^ SpEmptyPresenter
]

{ #category : #running }
SpLayoutTest >> testInitialLayoutIsEmpty [

	self assert: layout isEmpty
]

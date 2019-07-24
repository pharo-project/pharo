Class {
	#name : #CBAbstractButtonActionTest,
	#superclass : #CBAbstractTest,
	#instVars : [
		'criticBrowserPresenter'
	],
	#category : #'Tool-CriticBrowser-Tests-Tests'
}

{ #category : #testing }
CBAbstractButtonActionTest class >> isAbstract [ 
	^ self = CBAbstractButtonActionTest 
]

Class {
	#name : #CBAbstractButtonActionTest,
	#superclass : #CBAbstractTest,
	#instVars : [
		'criticBrowserPresenter'
	],
	#category : #'Tool-CriticBrowser-Tests-Test-Button'
}

{ #category : #testing }
CBAbstractButtonActionTest class >> isAbstract [ 
	^ self = CBAbstractButtonActionTest 
]

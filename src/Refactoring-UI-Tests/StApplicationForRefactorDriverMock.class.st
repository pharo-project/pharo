"
A mock application used by refactoring driver tests.

It records the last message received through #inform: so tests can assert the feedback reported to the user without opening any dialog. It is installed on a driver through the #application: hook.
"
Class {
	#name : 'StApplicationForRefactorDriverMock',
	#superclass : 'Object',
	#instVars : [
		'shownMessage'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'interaction' }
StApplicationForRefactorDriverMock >> inform: aString [

	shownMessage := aString
]

{ #category : 'accessing' }
StApplicationForRefactorDriverMock >> shownMessage [

	^ shownMessage
]

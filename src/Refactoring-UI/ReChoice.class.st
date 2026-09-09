Class {
	#name : 'ReChoice',
	#superclass : 'Object',
	#instVars : [
		'refactoring',
		'driver',
		'description'
	],
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReChoice >> description [

	^ description
]

{ #category : 'accessing' }
ReChoice >> description: aString [

	description := aString
]

{ #category : 'accessing' }
ReChoice >> driver [

	^ driver
]

{ #category : 'accessing' }
ReChoice >> driver: aDriver [

	driver := aDriver
]

{ #category : 'accessing' }
ReChoice >> refactoring [

	^ refactoring
]

{ #category : 'accessing' }
ReChoice >> refactoring: anObject [

	refactoring := anObject
]

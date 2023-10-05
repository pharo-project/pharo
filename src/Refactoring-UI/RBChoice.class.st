Class {
	#name : 'RBChoice',
	#superclass : 'Object',
	#instVars : [
		'refactoring',
		'driver'
	],
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBChoice >> driver [

	^ driver
]

{ #category : 'accessing' }
RBChoice >> driver: aDriver [

	driver := aDriver
]

{ #category : 'accessing' }
RBChoice >> refactoring [

	^ refactoring
]

{ #category : 'accessing' }
RBChoice >> refactoring: anObject [

	refactoring := anObject
]

"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : #SycClassCmCommand,
	#superclass : #SycCmCommand,
	#instVars : [
		'targetClass'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #adding }
SycClassCmCommand class >> activationStrategy [ 
	^ SycClassMenuActivation
]

{ #category : #executing }
SycClassCmCommand >> applyCommandResult [
]

{ #category : #executing }
SycClassCmCommand >> execute [ 
	[self prepareFullExecution.
	self executeRefactoring.
	self applyCommandResult] 
		on: Exception do: [ :exc | self processCommandFailure: exc ]
]

{ #category : #executing }
SycClassCmCommand >> executeRefactoring [
]

{ #category : #'as yet unclassified' }
SycClassCmCommand >> prepareFullExecution [
	targetClass := context lastSelectedClass.
]

{ #category : #execution }
SycClassCmCommand >> processCommandFailure: anException [
	
	context processFailure: anException of: self
]

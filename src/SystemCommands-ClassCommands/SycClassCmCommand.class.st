"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : 'SycClassCmCommand',
	#superclass : 'SycCmCommand',
	#instVars : [
		'targetClass',
		'refactoringScopes'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'adding' }
SycClassCmCommand class >> activationStrategy [
	^ SycClassMenuActivation
]

{ #category : 'preparation' }
SycClassCmCommand >> prepareFullExecution [
	super prepareFullExecution.
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass
]

{ #category : 'accessing' }
SycClassCmCommand >> targetClass [

	^ targetClass
]

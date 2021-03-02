"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : #SycClassCmCommand,
	#superclass : #SycCmCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #adding }
SycClassCmCommand class >> activationStrategy [ 
	^ SycClassMenuActivation
]

"
I implement commands to create scopes from classes and class hierarchies in Calypso
"
Class {
	#name : 'SycScopesCmCommand',
	#superclass : 'SycCmCommand',
	#category : 'SystemCommands-ClassCommands-Scope',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Scope'
}

{ #category : 'adding' }
SycScopesCmCommand class >> activationStrategy [
	^ SycClassMenuActivation 
]

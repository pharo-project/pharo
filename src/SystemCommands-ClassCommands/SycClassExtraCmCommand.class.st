"
Root class to group Extra commands applied to selected classes in a tool
"
Class {
	#name : 'SycClassExtraCmCommand',
	#superclass : 'SycCmCommand',
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'execution' }
SycClassExtraCmCommand class >> activationStrategy [

	^ SycExtraMenuActivation
]

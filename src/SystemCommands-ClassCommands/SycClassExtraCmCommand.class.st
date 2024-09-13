Class {
	#name : 'SycClassExtraCmCommand',
	#superclass : 'SycCmCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycClassExtraCmCommand class >> activationStrategy [

	^ SycExtraMenuActivation
]

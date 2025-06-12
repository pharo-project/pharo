Class {
	#name : 'StPlaygroundPreviousProfStefCommand',
	#superclass : 'StPlaygroundCommand',
	#category : 'ProfStef-Core-command',
	#package : 'ProfStef-Core',
	#tag : 'command'
}

{ #category : 'default' }
StPlaygroundPreviousProfStefCommand class >> defaultDescription [

	^ 'go to the previous lessons'
]

{ #category : 'accessing' }
StPlaygroundPreviousProfStefCommand class >> defaultIconName [

	^ #glamorousLeft
]

{ #category : 'default' }
StPlaygroundPreviousProfStefCommand class >> defaultName [

	^ 'previous lesson'
]

{ #category : 'executing' }
StPlaygroundPreviousProfStefCommand >> execute [
	ProfStef previous.
]

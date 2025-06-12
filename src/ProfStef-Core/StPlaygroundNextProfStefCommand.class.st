Class {
	#name : 'StPlaygroundNextProfStefCommand',
	#superclass : 'StPlaygroundCommand',
	#category : 'ProfStef-Core-command',
	#package : 'ProfStef-Core',
	#tag : 'command'
}

{ #category : 'default' }
StPlaygroundNextProfStefCommand class >> defaultDescription [

	^ 'go to the next lessons'
]

{ #category : 'accessing' }
StPlaygroundNextProfStefCommand class >> defaultIconName [

	^ #glamorousRight 
]

{ #category : 'default' }
StPlaygroundNextProfStefCommand class >> defaultName [

	^ 'Next lesson'
]

{ #category : 'executing' }
StPlaygroundNextProfStefCommand >> execute [
	ProfStef next.
]

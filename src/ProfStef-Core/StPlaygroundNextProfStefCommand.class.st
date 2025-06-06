Class {
	#name : 'StPlaygroundNextProfStefCommand',
	#superclass : 'StPlaygroundDoItCommand',
	#category : 'ProfStef-Core-Command',
	#package : 'ProfStef-Core',
	#tag : 'Command'
}

{ #category : 'default' }
StPlaygroundNextProfStefCommand class >> defaultDescription [

	^ 'go to the next lessons'
]

{ #category : 'default' }
StPlaygroundNextProfStefCommand class >> defaultName [

	^ 'Next lesson'
]

{ #category : 'executing' }
StPlaygroundNextProfStefCommand >> execute [
	ProfStef next.
]

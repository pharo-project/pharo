Class {
	#name : 'StPlaygroundNextProfStefCommand',
	#superclass : 'StPlaygroundCommand',
	#category : 'ProfStef-Core-Command',
	#package : 'ProfStef-Core',
	#tag : 'Command'
}

{ #category : 'default' }
StPlaygroundNextProfStefCommand class >> defaultDescription [

	^ 'go to the next lesson'
]

{ #category : 'accessing' }
StPlaygroundNextProfStefCommand class >> defaultIconName [

	^ #right 
]

{ #category : 'default' }
StPlaygroundNextProfStefCommand class >> defaultName [

	^ 'Next lesson'
]

{ #category : 'executing' }
StPlaygroundNextProfStefCommand >> execute [
	ProfStef next.
]

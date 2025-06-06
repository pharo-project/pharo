Class {
	#name : 'StProfStefChoiseLesson',
	#superclass : 'SpPresenter',
	#instVars : [
		'listLesson',
		'tuto'
	],
	#category : 'ProfStef-Core-presenter',
	#package : 'ProfStef-Core',
	#tag : 'presenter'
}

{ #category : 'layout' }
StProfStefChoiseLesson >> defaultLayout [ 
	^ SpBoxLayout newTopToBottom.
]

{ #category : 'initialization' }
StProfStefChoiseLesson >> initialize [
	super initialize.
]

{ #category : 'initialization' }
StProfStefChoiseLesson >> initializePresenter [ 
	
]

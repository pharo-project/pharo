"
I'm a command use to ask sender of selected critic 
"
Class {
	#name : #CBSenderCriticCommand,
	#superclass : #CBCriticCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBSenderCriticCommand >> canBeRun [
	^ super canBeRun and: [ self criticEntity isCompiledMethod ]
]

{ #category : #hooks }
CBSenderCriticCommand >> execute [
	self systemNavigation browseAllSendersOf: self criticEntity selector
]

{ #category : #initialization }
CBSenderCriticCommand >> initialize [
	super initialize.
	self
		basicName: 'Senders';
		basicDescription: 'Browse the senders of the selected entity.'
]

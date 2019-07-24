Class {
	#name : #CBSenderCriticCommand,
	#superclass : #CBCriticCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBSenderCriticCommand >> canBeRun [
	^ super canBeRun and: [ self selectedCritic entity isCompiledMethod ]
]

{ #category : #hooks }
CBSenderCriticCommand >> execute [
	ClyQueryBrowser browseSendersOf: self selectedCritic entity selector
]

{ #category : #initialization }
CBSenderCriticCommand >> initialize [
	super initialize.
	self
		basicName: 'Senders';
		basicDescription: 'Browse the senders of the selected entity.'
]

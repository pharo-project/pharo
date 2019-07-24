"
i'm a command use to ask implementor of selected critic 
"
Class {
	#name : #CBImplementorsCriticCommand,
	#superclass : #CBCriticCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBImplementorsCriticCommand >> canBeRun [
	^ super canBeRun and: [ self selectedCritic entity isCompiledMethod ]
]

{ #category : #hooks }
CBImplementorsCriticCommand >> execute [
	ClyQueryBrowser browseImplementorsOf: self selectedCritic entity selector
]

{ #category : #initialization }
CBImplementorsCriticCommand >> initialize [
	super initialize.
	self
		basicName: 'Implementors';
		basicDescription: 'Browse the implementors of the selected entity.'
]

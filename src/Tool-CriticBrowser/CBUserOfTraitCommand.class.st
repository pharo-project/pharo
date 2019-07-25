"
I'm a command to ask users of the selected trait 
"
Class {
	#name : #CBUserOfTraitCommand,
	#superclass : #CBCriticCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBUserOfTraitCommand >> canBeRun [
	^ super canBeRun and: [ self criticEntity isTrait ]
]

{ #category : #hooks }
CBUserOfTraitCommand >> execute [
	self systemNavigation browseAllUsersOfTrait: self criticEntity
]

{ #category : #initialization }
CBUserOfTraitCommand >> initialize [
	super initialize.
	self
		basicName: 'Users of...';
		basicDescription: 'Browse the user of the selected trait.'
]

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
	^ super canBeRun and: [ self selectedCritic isTrait ]
]

{ #category : #hooks }
CBUserOfTraitCommand >> execute [
	SystemNavigation new browseAllUsersOfTrait: self selectedCritic
]

{ #category : #initialization }
CBUserOfTraitCommand >> initialize [
	super initialize.
	self
		basicName: 'usersOf...';
		basicDescription: 'Browse the user of the selected trait.'
]

"
I'm a command use to browse reference to the selectedClass
"
Class {
	#name : #CBReferenceToBehaviorCommand,
	#superclass : #CBCriticCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBReferenceToBehaviorCommand >> canBeRun [
	^ super canBeRun and: [ self selectedCritic isBehavior ]
]

{ #category : #hooks }
CBReferenceToBehaviorCommand >> execute [
	SystemNavigation new browseAllCallsOnClass: self selectedCritic
]

{ #category : #initialization }
CBReferenceToBehaviorCommand >> initialize [
	super initialize.
	self
		basicName: 'References to...';
		basicDescription: 'Browse reference of selected class.'
]

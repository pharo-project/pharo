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
	^ super canBeRun and: [ self criticEntity isBehavior ]
]

{ #category : #hooks }
CBReferenceToBehaviorCommand >> execute [
	self systemNavigation browseAllCallsOnClass: self criticEntity
]

{ #category : #initialization }
CBReferenceToBehaviorCommand >> initialize [
	super initialize.
	self
		basicName: 'References to...';
		basicDescription: 'Browse reference of selected class.'
]

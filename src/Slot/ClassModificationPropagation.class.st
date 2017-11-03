"
I represent a modification in a class that is produced because of a change in one of its superclasses. This is called a propagation.
"
Class {
	#name : #ClassModificationPropagation,
	#superclass : #AbstractClassModification,
	#instVars : [
		'origin'
	],
	#category : #Slot-ClassBuilder
}

{ #category : #'instance creation' }
ClassModificationPropagation class >> propagate: aModification to: aClass [
	^ self new
		propagate: aModification
		to: aClass
]

{ #category : #public }
ClassModificationPropagation >> apply [
	super apply.
	self copyTraitCompositionAndMethods.
	^ newClass
]

{ #category : #private }
ClassModificationPropagation >> copyTraitCompositionAndMethods [
	newClass traitComposition: self target traitComposition.
	(self target methods select: #isFromTrait)
		do: [ :m | newClass methodDict at: m selector put: m ].
	newClass traitComposition allTraits
		do: [ :t | 
			t removeUser: self target.
			t addUser: newClass ]
]

{ #category : #testing }
ClassModificationPropagation >> isPropagation [
	^true
]

{ #category : #migrating }
ClassModificationPropagation >> methodModification [
	^ origin methodModification
]

{ #category : #public }
ClassModificationPropagation >> propagate: aModification to: aTarget [
	origin := aModification.
	target := aTarget.
	self classLayout: (target classLayout 
		reshapeFrom: origin oldLayout slotScope
		to: origin newLayout).
]

{ #category : #accessing }
ClassModificationPropagation >> superclass [
	^ origin newClass
]

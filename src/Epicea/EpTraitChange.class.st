"
I represent a change in the definition of a trait.
"
Class {
	#name : #EpTraitChange,
	#superclass : #EpBehaviorChange,
	#category : 'Epicea-Model'
}

{ #category : #testing }
EpTraitChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpTraitChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]
]

{ #category : #testing }
EpTraitChange >> isEpTraitChange [

	^ true
]

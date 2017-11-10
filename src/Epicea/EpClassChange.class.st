"
I represent a change in the definition of a class.
"
Class {
	#name : #EpClassChange,
	#superclass : #EpBehaviorChange,
	#category : #Epicea-Model
}

{ #category : #testing }
EpClassChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpClassChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]
]

{ #category : #testing }
EpClassChange >> isEpClassChange [

	^ true
]

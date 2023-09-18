"
I represent a change in the definition of a class.
"
Class {
	#name : 'EpClassChange',
	#superclass : 'EpBehaviorChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'testing' }
EpClassChange class >> isAbstract [

	^ self == EpClassChange
]

{ #category : 'testing' }
EpClassChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpClassChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]
]

{ #category : 'testing' }
EpClassChange >> isEpClassChange [

	^ true
]

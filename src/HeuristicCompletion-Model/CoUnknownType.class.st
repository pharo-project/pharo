"
I represent an unknown type for CoTypeInferencer.
"
Class {
	#name : 'CoUnknownType',
	#superclass : 'Object',
	#category : 'HeuristicCompletion-Model-InitializeInferenceHeuristic',
	#package : 'HeuristicCompletion-Model',
	#tag : 'InitializeInferenceHeuristic'
}

{ #category : 'comparing' }
CoUnknownType >> = aType [

	^ aType isKindOf: CoUnknownType
]

{ #category : 'comparing' }
CoUnknownType >> hash [

	^ self class hash
]

{ #category : 'accessing' }
CoUnknownType >> instanceSide [

	^ self
]

{ #category : 'lookup' }
CoUnknownType >> lookupSelector: aString [

	^ nil
]

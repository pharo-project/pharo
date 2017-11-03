"
This filter only accepts code changes that would produce a change if applied in current image.
"
Class {
	#name : #EpImpactCodeChangeFilter,
	#superclass : #EpFilter,
	#instVars : [
		'environment'
	],
	#category : #EpiceaBrowsers-Filters
}

{ #category : #accessing }
EpImpactCodeChangeFilter class >> title [

	^ 'Hide changes if apply operation is redundant'
]

{ #category : #comparing }
EpImpactCodeChangeFilter >> = other [

	^ self class = other class
]

{ #category : #filtering }
EpImpactCodeChangeFilter >> accepts: anOmEntry [

	anOmEntry content isCodeChange ifFalse: [ ^false ].

	^ anOmEntry content accept: (EpHasImpactVisitor inEnvironment: environment)
]

{ #category : #accessing }
EpImpactCodeChangeFilter >> environment: anEnvironment [
	
	environment := anEnvironment
]

{ #category : #comparing }
EpImpactCodeChangeFilter >> hash [

	^ self class hash
]

{ #category : #accessing }
EpImpactCodeChangeFilter >> title [
	
	^ self class title
]

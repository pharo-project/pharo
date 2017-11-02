"
I represent a change in a code entity.
"
Class {
	#name : #EpCodeChange,
	#superclass : #EpEvent,
	#category : 'Epicea-Model'
}

{ #category : #accessing }
EpCodeChange >> affectedPackageName [

	^ self subclassResponsibility 
]

{ #category : #testing }
EpCodeChange >> doesOverride: aCodeChange [ 

	^ self subclassResponsibility
]

{ #category : #testing }
EpCodeChange >> isCodeChange [

	^ true
]

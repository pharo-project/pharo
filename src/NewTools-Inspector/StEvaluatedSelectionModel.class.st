Class {
	#name : #StEvaluatedSelectionModel,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #'instance creation' }
StEvaluatedSelectionModel class >> newValue: anObject [ 

	^ self new
		value: anObject;
		yourself
]

{ #category : #'api-selection' }
StEvaluatedSelectionModel >> selectedItem [

	^ self value
]

{ #category : #accessing }
StEvaluatedSelectionModel >> value [

	^ value
]

{ #category : #accessing }
StEvaluatedSelectionModel >> value: anObject [

	value := anObject
]

"
A selection container to simulate a selection object (the object that is sent when a selection is triggered on `activation` and `selection` events). 
This is used so we can trigger `activation` events outside lists tables and trees (for example, in a playground or an inpector evaluator).   
"
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

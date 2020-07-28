Class {
	#name : #StInspection,
	#superclass : #SpPresenter,
	#instVars : [
		'model'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #converting }
StInspection >> asInspection [

	^ self
]

{ #category : #testing }
StInspection >> hasOutputActivationPort [

	^ false
]

{ #category : #'model access' }
StInspection >> model [
	^ model
]

{ #category : #'model access' }
StInspection >> model: anInspectionModel [

	model := anInspectionModel
]

{ #category : #'model access' }
StInspection >> setModelBeforeInitialization: anInspectionModel [ 

	model := anInspectionModel
]

{ #category : #stepping }
StInspection >> step [
	"do nothing, it depends on my children"
]

"
A base inspection on an inspectionModel (which contains an inspected object).
Inspections are ways to see/inspect an object. 
They are the pages seen in the basic inspector.
"
Class {
	#name : #StInspection,
	#superclass : #SpPresenter,
	#instVars : [
		'model',
		'context'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #api }
StInspection >> activateOnDoubleClick [

]

{ #category : #api }
StInspection >> activateOnSingleClick [
]

{ #category : #converting }
StInspection >> asInspection [

	^ self
]

{ #category : #accessing }
StInspection >> context [

	^ context
]

{ #category : #accessing }
StInspection >> context: aContext [

	context := aContext
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

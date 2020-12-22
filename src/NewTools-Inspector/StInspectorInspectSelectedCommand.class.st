Class {
	#name : #StInspectorInspectSelectedCommand,
	#superclass : #StInspectorCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StInspectorInspectSelectedCommand class >> defaultDescription [

	^ 'Inspect this instance'
]

{ #category : #initialization }
StInspectorInspectSelectedCommand class >> defaultIconName [

	^ #smallInspectIt
]

{ #category : #default }
StInspectorInspectSelectedCommand class >> defaultName [
	<toolbarInspectCommand: 20>

	^ 'Inspect'
]

{ #category : #execution }
StInspectorInspectSelectedCommand >> execute [ 

	self interestingObject stInspect
]

{ #category : #execution }
StInspectorInspectSelectedCommand >> interestingObject [

	^ self context selectedObject
]

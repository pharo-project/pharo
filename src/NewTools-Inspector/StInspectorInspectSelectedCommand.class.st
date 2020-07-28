Class {
	#name : #StInspectorInspectSelectedCommand,
	#superclass : #StInspectorCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StInspectorInspectSelectedCommand class >> defaultDescription [

	^ 'Inspect this instance'
]

{ #category : #default }
StInspectorInspectSelectedCommand class >> defaultName [
	<toolbarInspectCommand: 20>

	^ 'Inspect'
]

{ #category : #converting }
StInspectorInspectSelectedCommand >> asSpecCommand [

	^ super asSpecCommand 
		iconName: #smallInspectIt;
		beDisplayedOnRightSide;
		yourself
]

{ #category : #execution }
StInspectorInspectSelectedCommand >> execute [ 

	self interestingObject stInspect
]

{ #category : #execution }
StInspectorInspectSelectedCommand >> interestingObject [

	^ self context selectedObject
]

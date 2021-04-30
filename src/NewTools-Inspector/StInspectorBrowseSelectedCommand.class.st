"
Browse current selected object.
"
Class {
	#name : #StInspectorBrowseSelectedCommand,
	#superclass : #StInspectorCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StInspectorBrowseSelectedCommand class >> defaultDescription [

	^ 'Browse this instance class definition'
]

{ #category : #default }
StInspectorBrowseSelectedCommand class >> defaultName [
	<toolbarInspectCommand: 30>

	^ 'Browse'
]

{ #category : #documentation }
StInspectorBrowseSelectedCommand class >> documentContextRequiredSelectors [

	^ super documentContextRequiredSelectors, #(selectedObject)
]

{ #category : #converting }
StInspectorBrowseSelectedCommand >> asSpecCommand [

	^ super asSpecCommand 
		iconName: #smallSystemBrowser;
		yourself
]

{ #category : #execution }
StInspectorBrowseSelectedCommand >> execute [ 

	self interestingObject browse
]

{ #category : #execution }
StInspectorBrowseSelectedCommand >> interestingObject [

	^ self context selectedObject
]

Class {
	#name : #StInspectorBrowseCommand,
	#superclass : #StInspectorCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StInspectorBrowseCommand class >> defaultDescription [

	^ 'Browse this instance class definition'
]

{ #category : #default }
StInspectorBrowseCommand class >> defaultName [
	<toolbarInspectCommand: 10>

	^ 'Browse'
]

{ #category : #converting }
StInspectorBrowseCommand >> asSpecCommand [

	^ super asSpecCommand 
		iconName: #smallSystemBrowser;
		beDisplayedOnRightSide;
		yourself
]

{ #category : #execution }
StInspectorBrowseCommand >> execute [ 

	self interestingObject browse
]

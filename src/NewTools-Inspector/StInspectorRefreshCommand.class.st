"
I'm a command to show the refresh content option for inspectors. 
I will call the #refreshContent method of my caller context.
"
Class {
	#name : #StInspectorRefreshCommand,
	#superclass : #StInspectorCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StInspectorRefreshCommand class >> defaultDescription [

	^ 'Refresh current panel'
]

{ #category : #default }
StInspectorRefreshCommand class >> defaultName [
	<toolbarInspectCommand: 30>

	^ 'Refresh'
]

{ #category : #converting }
StInspectorRefreshCommand >> asSpecCommand [

	^ super asSpecCommand 
		iconName: #glamorousRefresh;
		beDisplayedOnRightSide;
		yourself
]

{ #category : #execution }
StInspectorRefreshCommand >> execute [ 

	self context refreshContent
]

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

{ #category : #initialization }
StInspectorRefreshCommand class >> defaultIconName [

	^ #glamorousRefresh
]

{ #category : #default }
StInspectorRefreshCommand class >> defaultName [
	<toolbarInspectCommand: 10>

	^ 'Refresh'
]

{ #category : #execution }
StInspectorRefreshCommand >> execute [ 

	self context refreshContent
]

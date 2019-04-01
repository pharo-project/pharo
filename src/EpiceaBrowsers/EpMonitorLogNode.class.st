"
I am a node in a tree of Epicea logs that represent the EpMonitor current log. Read more in my superclass comment.
"
Class {
	#name : #EpMonitorLogNode,
	#superclass : #EpLogNode,
	#instVars : [
		'theLog',
		'headReference'
	],
	#category : #'EpiceaBrowsers-UI-Graph'
}

{ #category : #'instance creation' }
EpMonitorLogNode class >> for: anEpMonitor [
	self flag: #pharoFixMe.	"note: this is a workaround given this information is needed to build the references graph, but it's not available from 'theLog' when it's empty"

	^ self new
		theLog: anEpMonitor basicLog;
		headReference: anEpMonitor log headReference;
		yourself
]

{ #category : #converting }
EpMonitorLogNode >> asMorph [

	^ EpMorphFactory default emphasizedStringFor: self globalName, ' *'
]

{ #category : #accessing }
EpMonitorLogNode >> globalName [

	^ theLog store globalName
]

{ #category : #accessing }
EpMonitorLogNode >> headReference [
	^ headReference
]

{ #category : #accessing }
EpMonitorLogNode >> headReference: anObject [
	headReference := anObject
]

{ #category : #testing }
EpMonitorLogNode >> isMonitor [

	^ true
]

{ #category : #accessing }
EpMonitorLogNode >> log [
	^ theLog
]

{ #category : #accessing }
EpMonitorLogNode >> modificationTime [

	^ DateAndTime now
]

{ #category : #accessing }
EpMonitorLogNode >> referencedGlobalNamesIfEmptyLog [

	^ headReference isNull
		ifTrue: [ #() ] 
		ifFalse: [ { headReference globalName } ]
]

{ #category : #accessing }
EpMonitorLogNode >> theLog [
	^ theLog
]

{ #category : #accessing }
EpMonitorLogNode >> theLog: anObject [
	theLog := anObject
]

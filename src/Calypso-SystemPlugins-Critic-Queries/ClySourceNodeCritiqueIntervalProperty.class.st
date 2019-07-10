"
I represent problem part of source code using interval.
  
Internal Representation and Key Implementation Points.

    Instance Variables
	start:		<Integer>
	stop:		<Integer>
"
Class {
	#name : #ClySourceNodeCritiqueIntervalProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'start',
		'stop'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'instance creation' }
ClySourceNodeCritiqueIntervalProperty class >> from: anInterval [

	anInterval ifEmpty: [ ^ self new start: 0; stop: 0 ].

	^self new 
		start: anInterval first;
		stop: anInterval last
]

{ #category : #accessing }
ClySourceNodeCritiqueIntervalProperty >> start [
	^ start
]

{ #category : #accessing }
ClySourceNodeCritiqueIntervalProperty >> start: anObject [
	start := anObject
]

{ #category : #accessing }
ClySourceNodeCritiqueIntervalProperty >> stop [
	^ stop
]

{ #category : #accessing }
ClySourceNodeCritiqueIntervalProperty >> stop: anObject [
	stop := anObject
]

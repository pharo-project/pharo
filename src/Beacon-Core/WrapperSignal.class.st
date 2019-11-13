"
This signal is useful to log an arbitrary object.

Example:
	TranscriptLogger new runDuring: [
		42 asBeaconSignal emit.
	]. 
"
Class {
	#name : #WrapperSignal,
	#superclass : #BeaconSignal,
	#instVars : [
		'target'
	],
	#category : #'Beacon-Core-Signals'
}

{ #category : #'instance creation' }
WrapperSignal class >> emit: aTargetObject [
	^ (self on: aTargetObject) emit
]

{ #category : #'instance creation' }
WrapperSignal class >> log: aTargetObject [
	self deprecated: 'Should use #emit:'.
	^ self emit: aTargetObject 
]

{ #category : #'instance creation' }
WrapperSignal class >> on: aTargetObject [
	^ self new 
		target: aTargetObject 
]

{ #category : #accessing }
WrapperSignal >> name [
	^ 'Wrapper on ', self target class name
]

{ #category : #printing }
WrapperSignal >> printOneLineContentsOn: stream [
	self target printOn: stream 
]

{ #category : #accessing }
WrapperSignal >> target [
	^ target
]

{ #category : #accessing }
WrapperSignal >> target: anObject [
	target := anObject
]

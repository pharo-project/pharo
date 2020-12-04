Class {
	#name : #StHaltOnWriteCommand,
	#superclass : #StBreakpointCommand,
	#category : #'NewTools-ObjectCentricBreakpoints'
}

{ #category : #default }
StHaltOnWriteCommand class >> defaultDescription [

	^ 'Halts when the selected or any variable is written'
]

{ #category : #deafult }
StHaltOnWriteCommand class >> defaultIconName [

	^ #smallObjects
]

{ #category : #default }
StHaltOnWriteCommand class >> defaultName [ 

	^ 'Halt on write'
]

{ #category : #executing }
StHaltOnWriteCommand >> execute [
	^context haltInspectedObjectOnWriteAccess
]

Class {
	#name : #StHaltOnStateAccessCommand,
	#superclass : #StBreakpointCommand,
	#category : #'NewTools-ObjectCentricBreakpoints'
}

{ #category : #default }
StHaltOnStateAccessCommand class >> defaultDescription [

	^ 'Halts when the selected or any variable is read or written'
]

{ #category : #deafult }
StHaltOnStateAccessCommand class >> defaultIconName [
	^#remotesManagerEditRemote
]

{ #category : #default }
StHaltOnStateAccessCommand class >> defaultName [ 

	^ 'Halt on state access'
]

{ #category : #executing }
StHaltOnStateAccessCommand >> execute [
	^context haltInspectedObjectOnStateAccess
]

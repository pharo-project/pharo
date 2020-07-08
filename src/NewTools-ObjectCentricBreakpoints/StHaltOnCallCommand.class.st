Class {
	#name : #StHaltOnCallCommand,
	#superclass : #StBreakpointCommand,
	#category : #'NewTools-ObjectCentricBreakpoints'
}

{ #category : #default }
StHaltOnCallCommand class >> defaultDescription [

	^ 'Halts when this method is called'
]

{ #category : #default }
StHaltOnCallCommand class >> defaultName [ 

	^ 'Halt on call'
]

{ #category : #executing }
StHaltOnCallCommand >> execute [
	^context haltInspectedObjectOnCallToSelectedMethod
]

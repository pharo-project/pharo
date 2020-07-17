Class {
	#name : #StBreakpointCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-ObjectCentricBreakpoints'
}

{ #category : #deafult }
StBreakpointCommand class >> defaultIconName [
	^#smallDebug	
]

{ #category : #'instance creation' }
StBreakpointCommand class >> forSpec [ 
	^self forSpecWithIconNamed: self defaultIconName
]

{ #category : #testing }
StBreakpointCommand >> appliesTo: aTool [
	^ [ aTool owner class ~= StDebuggerRawObjectInspector ]
		on: Error
		do: [ false ]
]

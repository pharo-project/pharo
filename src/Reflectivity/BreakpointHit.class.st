"
I am announced by the system announcer when a breakpoint is hit during code execution.
I might carry an object, in the case the breakpoint that was hit was installed on a variable reading or an assignment.
In that case, the `#valueOrNil` accessor will give you access to the value of the variable.
If the breakpoint was installed on any other kind of node (_e.g._, a message send) the `#valueOrNil` accessor will return nil.

For more details about my structure, see my parent class `BreakpointAnnouncement`.
"
Class {
	#name : 'BreakpointHit',
	#superclass : 'BreakpointAnnouncement',
	#instVars : [
		'valueOrNil'
	],
	#category : 'Reflectivity-Breakpoints',
	#package : 'Reflectivity',
	#tag : 'Breakpoints'
}

{ #category : 'accessing' }
BreakpointHit >> valueOrNil [
	^ valueOrNil
]

{ #category : 'accessing' }
BreakpointHit >> valueOrNil: anObject [
	valueOrNil := anObject
]

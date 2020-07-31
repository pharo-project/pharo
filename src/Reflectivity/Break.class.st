"
I am a Breakpoint signal.

use class Breakpoint to insert me reflectively
"
Class {
	#name : #Break,
	#superclass : #Halt,
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #break }
Break class >> break [ 
	<debuggerCompleteToSender>
	self signal
]

{ #category : #break }
Break class >> break: aBreakpoint inContext: aContext node: node [
	<debuggerCompleteToSender>
	aBreakpoint class
		notifyBreakpointHit: aBreakpoint
		inContext: aContext
		node: node.
	aBreakpoint oneShot ifTrue: [ 
		aBreakpoint disabled ifTrue: [ ^ self ].
		aBreakpoint disable ].
	self break
]

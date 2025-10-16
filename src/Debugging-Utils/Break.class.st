"
I am a Breakpoint signal.

use class Breakpoint to insert me reflectively
"
Class {
	#name : 'Break',
	#superclass : 'Halt',
	#category : 'Debugging-Utils',
	#package : 'Debugging-Utils'
}

{ #category : 'break' }
Break class >> break [
	<debuggerCompleteToSender>
	self signal
]

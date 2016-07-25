signaling
pvtSignal: anException list: aList
	"Private. This method is used to signal an exception from another
	process...the receiver must be the active process.  If the receiver 
	was previously waiting on a Semaphore, then return the process
	to the waiting state after signaling the exception and if the Semaphore
	has not been signaled in the interim"

	"Since this method is not called in a normal way, we need to take care
	that it doesn't directly return to the caller (because I believe that could
	have the potential to push an unwanted object on the caller's stack)."

	| blocker |
	self isActiveProcess ifFalse: [^self].
	anException signal.
	blocker := Semaphore new.
	[self suspend.
	suspendedContext := suspendedContext swapSender: nil.
	aList class == Semaphore 
		ifTrue:
			[aList isSignaled
				ifTrue: 
					[aList wait.  "Consume the signal that would have restarted the receiver"
					self resume]
				ifFalse:
					["Add us back to the Semaphore's list (and remain blocked)"
					myList := aList.
					aList add: self]]
		ifFalse: [self resume]] fork.
	blocker wait.



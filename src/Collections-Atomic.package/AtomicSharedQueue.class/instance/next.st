accessing
next
	"Fetch the next item from queue. If queue is locked or empty, block the sender until operation can complete"
	 
	| result |
	
	[ | keepWaiting | 
		keepWaiting := false. 
		result := self nextIfNone: [ keepWaiting := true ]. 
		keepWaiting ] whileTrue: [ self waitForNewItems ].

	^ result
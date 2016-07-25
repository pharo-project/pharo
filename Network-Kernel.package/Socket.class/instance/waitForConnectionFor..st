waiting
waitForConnectionFor: timeout
	"Wait up until the given deadline for a connection to be established. Return true if it is established by the deadline, false if not."

	^self 
		waitForConnectionFor: timeout 
		ifTimedOut: [ConnectionTimedOut signal: 'Failed to connect in ', timeout asString, ' seconds']

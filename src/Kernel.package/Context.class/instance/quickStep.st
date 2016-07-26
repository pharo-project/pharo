system simulation
quickStep
	"If the next instruction is a send, just perform it.
	Otherwise, do a normal step."

	self willSend ifTrue: [ QuickStep := self ].
	^self step
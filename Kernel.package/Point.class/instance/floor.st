truncation and round off
floor
	"Answer a Point that is the receiver's x and y floor. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self ].
	^ x floor @ y floor

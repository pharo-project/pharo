truncation and round off
ceiling
	"Answer a Point that is the receiver's x and y ceiling. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self ].
	^ x ceiling @ y ceiling

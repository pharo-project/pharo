truncation and round off
truncated
	"Answer a Point whose x and y coordinates are integers. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self ].
	^ x truncated @ y truncated

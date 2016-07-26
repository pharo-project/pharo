actions
up
	self isUpEnabled ifFalse: [ ^ self ].
	self argumentList swap: self argumentIndex with: self argumentIndex - 1.
	self argumentIndex: self argumentIndex - 1
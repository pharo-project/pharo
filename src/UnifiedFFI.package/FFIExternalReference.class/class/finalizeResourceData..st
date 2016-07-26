finalization
finalizeResourceData: handle
	handle isNull ifTrue: [ ^ self ].
	handle free.
	handle beNull
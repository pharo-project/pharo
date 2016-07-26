deleting
destroy
	(texture isNil or: [ renderer isNil ])
		ifTrue: [ ^ self ].
	texture destroy.
	renderer destroy
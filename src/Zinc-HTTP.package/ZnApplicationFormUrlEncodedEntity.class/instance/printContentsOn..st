printing
printContentsOn: stream
	super printContentsOn: stream.
	self isEmpty
		ifTrue: [ ^ self ].
	stream space.
	fields printElementsOn: stream
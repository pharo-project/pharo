testing
includesSpan: span

	self spansDo: [ :each | each == span ifTrue: [ ^ true ] ].
	^ false
accessing
lastPane

	self subbricksCount = 0 ifTrue: [ ^ nil ].
	^ self panes last.
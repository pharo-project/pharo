accessing
visited

	visited == nil ifTrue: [ visited := IdentitySet new ].
	^visited
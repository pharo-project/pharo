public
nuke
	Instance notNil ifTrue: [ Instance release ].
	Instance := nil
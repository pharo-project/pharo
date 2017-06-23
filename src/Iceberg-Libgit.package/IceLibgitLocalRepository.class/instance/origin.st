accessing
origin
	| remotes |
	remotes := self remotes.
	remotes ifEmpty: [ ^ nil  ].
	^ remotes
		detect: [ :each | each isOrigin ]
		ifNone: [ remotes first ]
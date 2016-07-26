dependencies
checkMissing
	| missing |
	missing := (self dependencies collect: [:ea | ea versionInfo name]) asSet.

	self repositories
		do: [:repo |
			repo allVersionNames
				do: [:found | missing remove: found ifAbsent: []]]
		displayingProgress: 'searching versions'.

	^missing isEmpty or: [
		self selectDependency: missing anyOne.
		self confirm: (String streamContents: [:strm |
			strm nextPutAll: 'No repository found for'; cr.
			missing do: [:r | strm nextPutAll: r; cr].
			strm nextPutAll: 'Do you still want to store?'])]
	
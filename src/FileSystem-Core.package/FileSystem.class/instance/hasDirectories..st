public-testing
hasDirectories: aResolvable
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | 
			entry isDirectory ifTrue: [ ^true ] ].
	^false
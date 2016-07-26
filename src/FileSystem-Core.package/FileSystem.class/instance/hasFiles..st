public-testing
hasFiles: aResolvable
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | entry isFile ifTrue: [ ^true ] ].
	^false
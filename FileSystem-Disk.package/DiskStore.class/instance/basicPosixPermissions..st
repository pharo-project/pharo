private
basicPosixPermissions: anEntry
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].
private
basicIsSymlink: anEntry
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]
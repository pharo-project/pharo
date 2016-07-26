private
flushBuffer
	position = 0 ifTrue: [ ^ self ].
	position = buffer size
		ifTrue: [
			stream nextPutAll: buffer ]
		ifFalse: [
			streamRespondsToNextPutAllStartingAt
				ifTrue: [ stream next: position putAll: buffer startingAt: 1 ]
				ifFalse: [ stream nextPutAll: (buffer copyFrom: 1 to: position) ] ].
	position := 0
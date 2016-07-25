private
flushBuffer
	position = 0 ifTrue: [ ^ self ].
	position = self bufferSize
		ifTrue: [
			stream nextPutAll: buffer ]
		ifFalse: [
			(stream respondsTo: #next:putAll:startingAt:)
				ifTrue: [ stream next: position putAll: buffer startingAt: 1 ]
				ifFalse: [ stream nextPutAll: (buffer copyFrom: 1 to: position) ] ].
	position := 0
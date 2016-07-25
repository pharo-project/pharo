writing
writeOn: outputStream
	(self contentLength isNil or: [ self contentLength = 0 ])
		ifTrue: [ 
			ZnUtils 
				streamFrom: self stream 
				to: outputStream ]
		ifFalse: [
			ZnUtils 
				streamFrom: self stream 
				to: outputStream 
				size: self contentLength ].
	stream close
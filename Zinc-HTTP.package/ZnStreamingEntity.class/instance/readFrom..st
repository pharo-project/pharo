initialize-release
readFrom: readStream
	self contentLength isNil
		ifTrue: [ 
			self stream: readStream ]
		ifFalse: [ | limitedStream |
			limitedStream := ZnLimitedReadStream on: readStream limit: self contentLength.
			self stream: limitedStream ]
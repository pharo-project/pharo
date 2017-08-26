initialize-release
readFrom: stream
	self contentLength isNil
		ifTrue: [ 
			self bytes: (ZnUtils readUpToEnd: stream limit: ZnMaximumEntitySize value).
			self contentLength: self bytes size ]
		ifFalse: [ | byteArray |
			self contentLength > ZnMaximumEntitySize value
				ifTrue: [ ZnEntityTooLarge signal ].
			byteArray := ByteArray ofSize: self contentLength.
			self contentLength > ZnUtils streamingBufferSize 
				ifTrue: [
					ZnUtils streamFrom: stream to: byteArray writeStream size: self contentLength ]
				ifFalse: [
					stream next: self contentLength into: byteArray ]. 
			self bytes: byteArray ]
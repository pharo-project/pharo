private
readEntityFromStream
	| entity decodedEntityLength chunkedStream isChunked |
	chunkedStream := (isChunked := self isChunked) ifTrue: [ stream := ZnChunkedReadStream on: stream ] ifFalse: [ nil ].
	self isGzipped 
		ifTrue: [ 
			self hasContentLength ifTrue: [ stream := ZnLimitedReadStream on: stream limit: self contentLength ].
			decodedEntityLength := nil.
			stream := GZipReadStream on: stream ]
		ifFalse: [ decodedEntityLength := self contentLength ].
	entity := self readFrom: stream usingType: self contentType andLength: decodedEntityLength.
	isChunked ifTrue: [ 
		self isIdentityEncoded ifTrue: [ entity contentLength: stream totalSize ].
		chunkedStream extraHeaders ifNotNil: [ :extraHeaders | self headers addAll: extraHeaders ] ].
	^ entity
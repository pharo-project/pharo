initialize-release
readFrom: stream
	"Switch to streaming implementation later on"
	
	| bytes |
	"We have to use the contentLength instance variable instead of the accessor else a representation will be computed"
	contentLength isNil
		ifTrue: [ 
			bytes := ZnUtils readUpToEnd: stream limit: ZnMaximumEntitySize value ]
		ifFalse: [
			self contentLength > ZnMaximumEntitySize value ifTrue: [ ZnEntityTooLarge signal ].
			bytes := ByteArray ofSize: contentLength.
			stream next: contentLength into: bytes ].
	self parse: bytes boundary: self getBoundary asByteArray binary: false
memory space
spaceLeftAfterGarbageCollection
	"Return a string describing the amount of memory available"
	| availInternal availPhysical availTotal |
	self garbageCollect.
	availInternal := self primBytesLeft.
	availPhysical := self bytesLeft: false.
	availTotal := self bytesLeft: true.
	(availTotal > (availInternal + 10000)) "compensate for mini allocations inbetween"
		ifFalse:[^availInternal asStringWithCommas, ' bytes available'].
	^String streamContents:[:s|
		availInternal printWithCommasOn: s.
		s nextPutAll: ' bytes (internal) '; cr.
		 availPhysical printWithCommasOn: s.
		s nextPutAll: ' bytes (physical) '; cr.
		availTotal printWithCommasOn: s.
		s nextPutAll: ' bytes (total)     '].
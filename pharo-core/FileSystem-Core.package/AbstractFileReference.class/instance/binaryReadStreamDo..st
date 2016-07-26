streams-compatibility
binaryReadStreamDo: aBlock
	| stream |
	stream := self binaryReadStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
streams
writeStreamDo: aBlock
	| stream |
	stream := self writeStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
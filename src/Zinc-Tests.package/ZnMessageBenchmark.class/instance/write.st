accessing
write
	| writeStream |
	writeStream := self buffer writeStream.
	message writeOn: writeStream.
	^ writeStream
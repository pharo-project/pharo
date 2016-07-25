tests
testWriteStream
	| writeStream |
	self deny: preferences exists.
	self deny: preferences parent exists.
	writeStream := systemSettings writeStream.
	self assert: writeStream isStream.
	self assert: writeStream isEmpty.
	writeStream close.
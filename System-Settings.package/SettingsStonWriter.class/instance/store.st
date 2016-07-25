actions
store
	"Write the stored setting in the stream"

	stream ifNil: [ ^ self ].
	[ | writer |
	writer := STON writer
		on: stream;
		prettyPrint: true;
		asciiOnly: true;
		yourself.
	writer nextPut: storedSettings asArray ]
		ensure: [ stream close ]
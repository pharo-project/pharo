testing
testLargeCharacterStream
	"Regression test"

	| string |
	string := String streamContents: [ :out | 10 timesRepeat: [ out << Character alphabet ] ].
	hash := SHA1 hashStream: string readStream.
	self assert: hash hex equals: 'f9d5b271f9126e9051394cffaff0ae3250fd6087'
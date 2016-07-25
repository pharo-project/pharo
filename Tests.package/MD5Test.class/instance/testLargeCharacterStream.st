testing
testLargeCharacterStream
	"Regression test"

	| string |
	string := String streamContents: [ :out | 10 timesRepeat: [ out << Character alphabet ] ].
	hash := MD5 hashStream: string readStream.
	self assert: hash hex equals: '4e6405697346169610a3a39991c48321'
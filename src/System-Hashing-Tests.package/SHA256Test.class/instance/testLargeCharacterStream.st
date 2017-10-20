testing
testLargeCharacterStream
	"Regression test"

	| string |
	string := String streamContents: [ :out | 10 timesRepeat: [ out << Character alphabet ] ].
	hash := SHA256 hashStream: string readStream.
	self assert: hash hex equals: '6f6a4460cbd9241d7eff776ce2fd0439e0a865fb05b99f2c5f6f527f226e254d'
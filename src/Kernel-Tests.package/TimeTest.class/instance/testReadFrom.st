tests - input
testReadFrom
	"self debug: #testReadFrom"
	
	| string t |
	string := '4:02:47 am'.
	t := self timeClass readFrom: string readStream.
	self assert: time printString = t printString.

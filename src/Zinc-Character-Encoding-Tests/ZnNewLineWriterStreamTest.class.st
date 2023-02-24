Class {
	#name : #ZnNewLineWriterStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #tests }
ZnNewLineWriterStreamTest >> testClose [
	| string result znstream |
	string := 'abcčřž' , String cr.
	result := String streamContents: [ :out |
		znstream := ZnNewLineWriterStream on: (ZnBufferedWriteStream on: out).
		znstream forLf.
		znstream nextPutAll: string.
		znstream close ].
	string at: string size put: Character lf.
	self assert: result equals: string
]

{ #category : #tests }
ZnNewLineWriterStreamTest >> testNextPut [
	"Ensure that the line ends are written correctly"

	| expectedString stream crStream |

	expectedString := 'a', OSPlatform current lineEnding, 'b'.
	{ String cr.
		String lf.
		String crlf. } do: [ :lineEnd |
			stream := String new writeStream.
			crStream := ZnNewLineWriterStream on: stream.
			crStream
				<< 'a';
				<< lineEnd;
				<< 'b'.
			self assert: stream contents equals: expectedString ]
]

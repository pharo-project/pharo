Class {
	#name : #ZnNewLineWriterStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #tests }
ZnNewLineWriterStreamTest >> testClose [ 

	| string fs fileReference znstream |

	fs := FileSystem memory.
	fileReference := fs / 'test.txt'.
	string := String streamContents: [ :stream |
		stream 
			<< 'abcčřž';
			cr ].
	znstream := ZnNewLineWriterStream on: fileReference writeStream.
	znstream forLf.
	[ znstream nextPutAll: string ]
		ensure: [ znstream close ].
	string at: string size put: Character lf.
	self assert: fileReference contents equals: string.
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

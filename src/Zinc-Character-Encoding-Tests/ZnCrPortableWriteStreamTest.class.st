Class {
	#name : #ZnCrPortableWriteStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #tests }
ZnCrPortableWriteStreamTest >> testNextPut [
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

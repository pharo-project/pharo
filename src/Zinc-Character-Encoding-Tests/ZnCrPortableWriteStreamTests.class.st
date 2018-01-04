Class {
	#name : #ZnCrPortableWriteStreamTests,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #tests }
ZnCrPortableWriteStreamTests >> testNextPut [
	"Ensure that the line ends are written correctly"

	| expectedString stream crStream |

	expectedString := 'a', OSPlatform current lineEnding, 'b'.
	{ String cr.
		String lf.
		String crlf. } do: [ :lineEnd |
			stream := String new writeStream.
			crStream := ZnCrPortableWriteStream on: stream.
			crStream
				<< 'a';
				<< lineEnd;
				<< 'b'.
			self assert: stream contents equals: expectedString ]
]

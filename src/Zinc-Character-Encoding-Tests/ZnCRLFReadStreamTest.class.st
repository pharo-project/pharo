Class {
	#name : #ZnCRLFReadStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #tests }
ZnCRLFReadStreamTest >> testBasicStream [

	| lines input output |

	lines := #('Line 1' 'Line 2' 'Line 3').
	input := String streamContents: [ :stream |
		stream
			<< lines first;
			<< String cr;
			<< lines second;
			<< String lf;
			<< lines third;
			<< String crlf ].
	output := String streamContents: [ :stream |
		stream
			<< lines first;
			<< String crlf;
			<< lines second;
			<< String crlf;
			<< lines third;
			<< String crlf ].
	self
		assert: (ZnCRLFReadStream on: input asByteArray readStream) upToEnd
		equals: output asByteArray
]

{ #category : #tests }
ZnCRLFReadStreamTest >> testDoubleEnding [

	| lines input output |

	lines := #('Line 1' 'Line 2' 'Line 3').
	input := String streamContents: [ :stream |
		stream
			<< lines first;
			<< String cr;
			<< String cr;
			<< lines second;
			<< String lf;
			<< String lf;
			<< lines third;
			<< String crlf;
			<< String crlf ].
	output := String streamContents: [ :stream |
		stream
			<< lines first;
			<< String crlf;
			<< String crlf;
			<< lines second;
			<< String crlf;
			<< String crlf;
			<< lines third;
			<< String crlf;
			<< String crlf ].
	self
		assert: (ZnCRLFReadStream on: input asByteArray readStream) upToEnd
		equals: output asByteArray
]

{ #category : #tests }
ZnCRLFReadStreamTest >> testPeek [

	| input stream |

	input := String streamContents: [ :str |
		str
			<< 'a';
			<< String cr;
			<< 'b';
			<< String lf;
			<< 'c';
			<< String crlf ].
	stream := ZnCRLFReadStream on: input asByteArray readStream.
	self
		assert: stream peek
		equals: $a asInteger.
	self
		assert: stream next
		equals: $a asInteger.
	self
		assert: stream next
		equals: Character cr asInteger.
	self
		assert: stream peek
		equals: Character lf asInteger.
	self
		assert: stream next
		equals: Character lf asInteger.
	self
		assert: stream peek
		equals: $b asInteger.
	self
		assert: stream next
		equals: $b asInteger
]

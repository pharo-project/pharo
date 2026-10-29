Class {
	#name : 'ZnLineReaderTest',
	#superclass : 'TestCase',
	#category : 'Zinc-Tests',
	#package : 'Zinc-Tests'
}

{ #category : 'testing' }
ZnLineReaderTest >> testBinary [
	| input reader |
	input := ('Foo' , String crlf , 'Bar' , String crlf) asByteArray.
	reader := ZnLineReader on: input readStream.
	self assert: reader nextLine equals: 'Foo'.
	self assert: reader nextLine equals: 'Bar'.
	self assert: reader nextLine isEmpty
]

{ #category : 'testing' }
ZnLineReaderTest >> testLineLengthIsConfigurable [
	"A trusted API can send a header longer than the default limit, see svenvc/zinc#152"

	| input |
	input := (String new: (ZnCurrentOptions at: #maximumLineLength) + 1) atAllPut: $X.
	self
		should: [ (ZnLineReader on: input readStream) nextLine ]
		raise: ZnLineTooLong.
	(ZnOptions globalDefault clone
		at: #maximumLineLength put: input size + 1;
		yourself)
			during: [
				self
					assert: (ZnLineReader on: input readStream) nextLine
					equals: input ]
]

{ #category : 'testing' }
ZnLineReaderTest >> testLineTooLong [
	| input reader |
	input := (String new: 128) atAllPut: $X.
	reader := (ZnLineReader on: input readStream) limit: 64.
	self should: [ reader nextLine ] raise: ZnLineTooLong
]

{ #category : 'testing' }
ZnLineReaderTest >> testLineTooLongDefault [
	| input reader |
	input := (String new: (ZnCurrentOptions at: #maximumLineLength) + 1) atAllPut: $X.
	reader := ZnLineReader on: input readStream.
	self should: [ reader nextLine ] raise: ZnLineTooLong
]

{ #category : 'testing' }
ZnLineReaderTest >> testSimple [
	| input reader |
	input := 'Foo' , String crlf , 'Bar' , String crlf.
	reader := ZnLineReader on: input readStream.
	self assert: reader nextLine equals: 'Foo'.
	self assert: reader nextLine equals: 'Bar'.
	self assert: reader nextLine isEmpty
]

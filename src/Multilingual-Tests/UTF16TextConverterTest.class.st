"
SUnit tests for UTF16TextConverter 

"
Class {
	#name : #UTF16TextConverterTest,
	#superclass : #TestCase,
	#category : #'Multilingual-Tests-TextConversion'
}

{ #category : #helpers }
UTF16TextConverterTest >> assert: originalText isTheSameAfterLittleEndianConversion: aBooleanOrNil [

	"Test that encoding / decoding a string witll produce the original string for both byte orders"

	| converter bytes decodedText |

	converter := UTF16TextConverter new.
	aBooleanOrNil ifNotNil: [converter useLittleEndian: aBooleanOrNil].
	bytes := (originalText convertToWithConverter: converter).
	decodedText := bytes convertFromWithConverter: converter.
	
	self assert: originalText equals: decodedText.
	
]

{ #category : #helpers }
UTF16TextConverterTest >> assertBOMIsWrittenWithOrder: anOrder andConversion: aBlock [

	|converter stream  BOM  |
	
	converter := UTF16TextConverter new.
	converter useByteOrderMark: true.
	
	anOrder = #bigEndian ifTrue: [converter useLittleEndian: false].
	anOrder = #littleEndian ifTrue: [converter useLittleEndian: true].
	
	BOM := (converter useLittleEndian 
				ifTrue: [16rFFFE]
				ifFalse: [16rFEFF]) asByteArray asString.
	
	stream := String new writeStream. 
	aBlock value: converter value: stream.
	
	self assert: BOM equals: (stream contents  first: 2).
	
]

{ #category : #testing }
UTF16TextConverterTest >> testBOMWrite [
	
	"Test that BOM is written correctly for both endians, both when using a nextPut:, and nextPutAll:"
	
	| batchConversion oneByOneConversion |
	batchConversion := [:converter :stream | converter nextPutAll: 'test' toStream: stream].
	oneByOneConversion := [:converter :stream | 'test' do: [:each | converter nextPut: each toStream: stream]].
	
	self assertBOMIsWrittenWithOrder: #bigEndian 
		andConversion: oneByOneConversion.
	self assertBOMIsWrittenWithOrder: #bigEndian 
		andConversion: batchConversion.
		
	self assertBOMIsWrittenWithOrder: #littleEndian 
		andConversion: oneByOneConversion.
	self assertBOMIsWrittenWithOrder: #littleEndian 
		andConversion: batchConversion.
		
	self assertBOMIsWrittenWithOrder: nil 
		andConversion: oneByOneConversion.
	self assertBOMIsWrittenWithOrder: nil 
		andConversion: batchConversion.
]

{ #category : #testing }
UTF16TextConverterTest >> testByteOrders [

	"Test that encoding / decoding a string witll produce the original string for both byte orders"

	"Default endianness"
	self assert: 'test' isTheSameAfterLittleEndianConversion: nil.
	"Big endian"
	self assert: 'test' isTheSameAfterLittleEndianConversion: false.
	"Little endian"
	self assert: 'test' isTheSameAfterLittleEndianConversion: true.

]

{ #category : #testing }
UTF16TextConverterTest >> testNoBOMWriteInMiddleOfFile [
	
	"Test that BOM is not written when our first write does not occur at start of writestream"
	
	| converter stream |
	converter := UTF16TextConverter new.

	
	stream := String new writeStream.
	converter nextPutAll: 'testing' toStream: stream.
	stream position: 2.
	converter useByteOrderMark: true.
		
	converter nextPut: $a toStream: stream.
	self assert: 0 equals:  (stream contents indexOfSubCollection: 16rFEFF asByteArray asString)
]

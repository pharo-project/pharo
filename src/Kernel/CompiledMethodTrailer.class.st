"
I am responsible for encoding and decoding various kinds of compiled method trailer data.
I should not expose any binary data outside of myself, so all tools which working with compiled methods
should ask me to encode the meta-data, they want to be added to the compiled method trailer, as well as retrieve it.

To add a new kind of trailer, you should give it a proper name and define it in the #trailerKinds method at my class side.
Then you need to implement a corresponding #encode<your name> and #decode<your name> methods at instance side. Then add any public accessor methods, which will use a newly introduced trailer kind for communicating with outer layer(s).

An encodeXXX methods should store result (byte array) into encodedData instance variable.

A decodeXXX methods should read the data from compiled method instance, held by 'method' ivar,
and always set 'size' ivar (denoting a total length of trailer in compiled method) and optionally 'data' ivar which should keep a decoded data, ready to be used by outer layer(s) using accessor method(s) you providing.

The kind of compiled method trailer is determined by the last byte of compiled method.

The byte format used is following: 
	""2rkkkkkkdd""

where 'k' bits stands for 'kind' , allowing totally 64 different kinds of method trailer
and 'd' bits is data.

Following is the list of currently defined trailer kinds:

NoTrailer , k = 000000, dd unused
method has no trailer, and total trailer size bytes is always 1

ClearedTrailer, k = 000001, 
method has cleared trailer (it was set to something else, but then cleared) 
dd+1  determines the number of bytes for size field, and size is a total length of trailer bytes
So a total length of trailer is: 1 + (dd + 1) + size

TempsNamesQCompress, k = 000010
the trailer contains a list of method temp names,  compressed using qCompress: method. 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer.
So a total length of trailer is:  1 + (dd + 1) + size

TempsNamesZip, k = 000011
the trailer contains a list of method temp names,  compressed using GZIP compression method. 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer
So a total length of trailer is: 1 + (dd + 1) + size

SourceBySelector, k = 000100
the trailer indicates , that method source is determined by a class + selector where it is installed to. 
Trailer size = 1.

SourceByStringIdentifier, k = 000101
the trailer indicates , that method source is determined by a class + some ByteString identifier. 
dd+1  determines the number of bytes for size of ByteString identifier, and size is number of bytes of string.
A total length of trailer is:  1 + (dd + 1) + size

EmbeddedSourceQCompress, k = 000110
the trailer contains an utf-8 encoded method source code, compressed using qCompress method
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed source code
A total length of trailer is:  1 + (dd + 1) + size

EmbeddedSourceZip, k = 000111
the trailer contains an utf-8 encoded method source code, comressed using GZIP 
dd+1  determines the number of bytes for size field, and size is a number of bytes of compressed buffer
A total length of trailer is:  1 + (dd + 1) + size

VarLengthSourcePointer, k = 001000
the trailer is variable-length encoded source pointer. 
dd bits is unused.

ExtendedKind, k = 001001
the next byte of trailer (one that prepends the last byte of compiled method)
denotes an extended kind of trailer, allowing to use additional 256 kinds of encoding method's trailer in future. 

SourcePointer, k = 111111 
the trailer is encoded source pointer. Total trailer size is 4-bytes 
(this kind of encoding is backwards compatible with most of existing compiled methods)


"
Class {
	#name : #CompiledMethodTrailer,
	#superclass : #Object,
	#instVars : [
		'data',
		'encodedData',
		'kind',
		'size',
		'method'
	],
	#category : #'Kernel-Methods'
}

{ #category : #kinds }
CompiledMethodTrailer class >> empty [
	"answer the empty trailer"
	^ self new
]

{ #category : #generated }
CompiledMethodTrailer class >> trailerKindDecoders [

	^#(#decodeNoTrailer #decodeClearedTrailer #decodeTempsNamesQCompress #decodeTempsNamesZip #decodeSourceBySelector #decodeSourceByStringIdentifier #decodeEmbeddedSourceQCompress #decodeEmbeddedSourceZip #decodeVarLengthSourcePointer #decodeExtendedKind #decodeNativeCodeTrailer #decodeEmbeddedSource #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeUndefined #decodeSourcePointer)
]

{ #category : #kinds }
CompiledMethodTrailer class >> trailerKinds [
	" see class comment for description."
	
	^#(
"000000" #NoTrailer
"000001" #ClearedTrailer
"000010" #TempsNamesQCompress
"000011" #TempsNamesZip
"000100" #SourceBySelector
"000101" #SourceByStringIdentifier
"000110" #EmbeddedSourceQCompress
"000111" #EmbeddedSourceZip
"001000" #VarLengthSourcePointer
"001001" #ExtendedKind
"001010" #NativeCodeTrailer  "this number is hardcoded in VM"
"001011" #EmbeddedSource
"001100" #EmbeddedSourceWide
"001101" #Undefined
"001110" #Undefined
"001111" #Undefined
"010000" #Undefined
"010001" #Undefined
"010010" #Undefined
"010011" #Undefined
"010100" #Undefined
"010101" #Undefined
"010110" #Undefined
"010111" #Undefined
"011000" #Undefined
"011001" #Undefined
"011010" #Undefined
"011011" #Undefined
"011100" #Undefined
"011101" #Undefined
"011110" #Undefined
"011111" #Undefined
"100000" #Undefined
"100001" #Undefined
"100010" #Undefined
"100011" #Undefined
"100100" #Undefined
"100101" #Undefined
"100110" #Undefined
"100111" #Undefined
"101000" #Undefined
"101001" #Undefined
"101010" #Undefined
"101011" #Undefined
"101100" #Undefined
"101101" #Undefined
"101110" #Undefined
"101111" #Undefined
"110000" #Undefined
"110001" #Undefined
"110010" #Undefined
"110011" #Undefined
"110100" #Undefined
"110101" #Undefined
"110110" #Undefined
"110111" #Undefined
"111000" #Undefined
"111001" #Undefined
"111010" #Undefined
"111011" #Undefined
"111100" #Undefined
"111101" #Undefined
"111110" #Undefined
"111111" #SourcePointer
	)
]

{ #category : #initialization }
CompiledMethodTrailer >> clear [
	kind := #NoTrailer.
	size := 4.
	data := encodedData := method := nil
]

{ #category : #accessing }
CompiledMethodTrailer >> compressSourceCode: aString [
	"Embed the source code into compiled method trailer, 
	pick best compression method"
	| temp |
	self clear.
	kind := #EmbeddedSourceQCompress.
	data := aString asString. 
	
	self encode.
	temp := encodedData.

	kind := #EmbeddedSourceZip.
	self encode.
	encodedData size > temp size ifTrue: [
		encodedData := temp.
		kind := #EmbeddedSourceQCompress.
		size := encodedData size.
		]
]

{ #category : #accessing }
CompiledMethodTrailer >> compressedSourceCode [
	"Answer the source code of compiled method.
	Note: it does not attempts to read from source files using sourcePointer,
	nor reconstruct the source code using temp names"
	
	kind == #EmbeddedSourceQCompress ifTrue: [^ data ].
	kind == #EmbeddedSourceZip ifTrue: [^ data ].
	^ nil
]

{ #category : #'creating a method' }
CompiledMethodTrailer >> createMethod: numberOfBytesForAllButTrailer class: aCompiledMethodClass header: headerWord [ 
	| meth |
	"Instantiates a compiled method with this trailer.
	- numberOfBytesForAllButTrailer specifies the number of bytes needed to encode the literals and the bytecode.
	- aCompiledMethodClass specifies the concrete class of the compiled method.
	- headerWord specifies the header used for the compiled method.
	
	This method will create a compiled method instance of the correct size, set it's header and trailing bytes. The bytecode and literals must be set by the caller."
	
	encodedData ifNil: [ self encode ].
	
	meth := aCompiledMethodClass newMethod: numberOfBytesForAllButTrailer + size header: headerWord.
	"copy the encoded trailer data"
	1 to: size do:
		[:i | meth at: meth size - size + i put: (encodedData at: i)].

	^meth
]

{ #category : #decoding }
CompiledMethodTrailer >> decodeClearedTrailer [
	"Size is set in #decodeLengthField"

	self decodeLengthField.

]

{ #category : #decoding }
CompiledMethodTrailer >> decodeEmbeddedSource [
	"data is string with method's source code"

	| len start end newSize byteArray |
	len := self decodeLengthField.
	start := method size - self size + 1.
	end := method size - self size + len.
	newSize := end - start + 1. 
	byteArray := (ByteArray new: newSize)
		replaceFrom: 1
		to: newSize
		with: method
		startingAt: start.
	data := byteArray asString
]

{ #category : #decoding }
CompiledMethodTrailer >> decodeEmbeddedSourceQCompress [ 
	"Trailer is compressed string using qCompress method + length field + 1 byte
	
	Decompress strings compressed by qCompress:.
	Most common 11 chars get values 0-10 packed in one 4-bit nibble;
	next most common 52 get values 12-15 (2 bits) * 16 plus next nibble;
	escaped chars get three nibbles"
	| len str input |
	
	len := self decodeLengthField.
 	input := (ReadStream on: method from: method size - size+1 to: method size - size + len).
	
	str := String streamContents:
		[:strm | | nextNibble nibble peek charTable |
		charTable :=  "Character encoding table must match qCompress:"
		'ear tonsilcmbdfghjkpquvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345[]()'.
		peek := true.
		nextNibble := [peek
						ifTrue: [peek := false. input peek ifNil: [0] ifNotNil: [:b| b // 16]]
						ifFalse: [peek := true. input next ifNil: [0] ifNotNil: [:b| b \\ 16]]].
		[input atEnd] whileFalse:
			[(nibble := nextNibble value) = 0
				ifTrue: [input atEnd ifFalse:
						[strm nextPut: (Character value: nextNibble value * 16 + nextNibble value)]]
				ifFalse:
					[nibble <= 11
						ifTrue:
							[strm nextPut: (charTable at: nibble)]
						ifFalse:
							[strm nextPut: (charTable at: (nibble-12) * 16 + nextNibble value)]]]].
			
	data := str convertFromEncoding: 'utf8'
]

{ #category : #decoding }
CompiledMethodTrailer >> decodeEmbeddedSourceWide [
	"data is string with method's source code"

	| len start end newSize byteArray |
	len := self decodeLengthField.
	start := method size - self size + 1.
	end := method size - self size + len.
	newSize := end - start + 1. 
	byteArray := (ByteArray new: newSize)
		replaceFrom: 1
		to: newSize
		with: method
		startingAt: start.
	data := byteArray utf8Decoded
]

{ #category : #encoding }
CompiledMethodTrailer >> decodeExtendedKind [

	"reserved for future use"
	self shouldBeImplemented.

]

{ #category : #private }
CompiledMethodTrailer >> decodeLengthField [

	"used in various encodings, where length field is preceeding the last trailer byte.
	Two least significant bits in last byte denoting the number of bytes for length field"
	
	| numBytes pos length |

	pos := method size.
	numBytes := ((method at: pos) bitAnd: 3) + 1.
	
	length := 0.
	1 to: numBytes do: [:i |
		length := length << 8 + (method at: pos - i ).
		].
	size := 1 + numBytes + length.
	
	^ length
]

{ #category : #decoding }
CompiledMethodTrailer >> decodeNoTrailer [
	"Not much to decode here"
	size := 4. 
]

{ #category : #decoding }
CompiledMethodTrailer >> decodeSourcePointer [
	"Trailer is a source pointer"
	| msz |
	
	size := 4.
	msz := method size.
	data := (method at: msz) - 251 << 8 + (method at: msz-1) << 8
	 + (method at: msz-2) << 8 + (method at: msz-3).
	

]

{ #category : #decoding }
CompiledMethodTrailer >> decodeUndefined [
	self error: 'undefined method encoding'
]

{ #category : #decoding }
CompiledMethodTrailer >> decodeVarLengthSourcePointer [

	| pos shift |
	
	pos := method size-1.
	shift := data := 0.
	
	[ | value | 
		value := method at: pos.
		data := (value bitAnd: 16r7F) << shift + data.
		pos := pos - 1.
		shift := shift + 7.
		value > 127 ] whileTrue.

	size := method size - pos.
]

{ #category : #accessing }
CompiledMethodTrailer >> embeddedSourceCode [
	"Answer the already decoded source code of compiled method"
	
	^ data
]

{ #category : #accessing }
CompiledMethodTrailer >> embeddedSourceCode: aString [
	"Embed the source code into compiled method trailer"

	self clear.
	
	data := aString asString.
	kind := data isWideString  
		ifTrue: [#EmbeddedSourceWide]
		ifFalse: [#EmbeddedSource].
	self encode.
]

{ #category : #encoding }
CompiledMethodTrailer >> encode [

	encodedData := nil.
	
	"encode the trailer into byte array"
	self perform: ('encode' , kind) asSymbol.

	[encodedData notNil and: [encodedData size > 0 ]] assert.
	
	"set the size"
	size := encodedData size.
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeClearedTrailer [

	"A cleared trailer is replaced by #NoTrailer, when used for encoding"
	self clear.
	kind := #NoTrailer.
	
	^ self encode
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeEmbeddedSource [
	"data is string with method's source code"

	| length str encodedLength | 
	length := data size.
	str := WriteStream on: (ByteArray new: length).
	encodedLength := self encodeLengthField: length.
	str 
		nextPutAll: data asByteArray;
		nextPutAll: encodedLength;	"trailing byte"
	 	nextPut: self kindAsByte + encodedLength size - 1.
	encodedData := str contents
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeEmbeddedSourceQCompress [
	"data is string with method's source code, encode it using qCompress method"

	| str length encodedLength |
	[ data isString ] assert.
	str := self qCompress: data.
	length := str position.
	encodedLength := self encodeLengthField: length.
	str nextPutAll: encodedLength.	"trailing byte"
	str nextPut: self kindAsByte + encodedLength size - 1.
	encodedData := str contents
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeEmbeddedSourceWide [
	"data is string with method's source code"

	| str length encodedLength utf8data |
	[ data isString ] assert.
	utf8data := data utf8Encoded.
	str := WriteStream on: (ByteArray new: utf8data size).
	length := utf8data size.
	encodedLength := self encodeLengthField: length.
	str nextPutAll: utf8data asByteArray.
	str nextPutAll: encodedLength.	"trailing byte"
	str nextPut: self kindAsByte + encodedLength size - 1.
	encodedData := str contents.
]

{ #category : #private }
CompiledMethodTrailer >> encodeLengthField: integer [
	| bytes value |
	[integer > 0] assert.
	value := integer.
	bytes := ByteArray streamContents: [:str |
		[ value > 0 ] whileTrue: [
			str nextPut: (value bitAnd: 255).
			value := value >> 8 ]].
	"no more than 4 bytes for length field"
	[bytes size <=4] assert.
	^ bytes
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeNoTrailer [

	"When creating methods with no trailer, male them, 4 bytes long so that we can update them to sourcePointers 
	without having to use #become:"

	size := 4.
	encodedData := #[0 0 0 0].
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeSourcePointer [

	encodedData := ByteArray new: 4.
	encodedData at: 4 put: (data >> 24) + 251.
	1 to: 3 do: [:i |
		encodedData at: 4-i put: ((data bitShift: (i-3)*8) bitAnd: 16rFF)]
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeUndefined [

	self error: 'use of an undefined kind of trailer encoding'
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeVarLengthSourcePointer [

	"source pointer must be >=0"
	[data >= 0] assert.
	
	encodedData := 
		data = 0 ifTrue: [ #[0] ] 
		ifFalse: [ ByteArray streamContents: [:str |
		| value |
		value := data.
		[value > 0] whileTrue: [
			value > 127 ifTrue: [ str nextPut: 128 + (value bitAnd: 16r7F) ]
				ifFalse: [ str nextPut: value. ].
			value := value >> 7.
			].
		]].
	encodedData := encodedData reversed copyWith: (self kindAsByte)
]

{ #category : #accessing }
CompiledMethodTrailer >> endPC [
	"Answer the index of the last bytecode."
	
	method ifNil: [ self error: 'Cannot determine the endPC without compiled method' ].
	
	"if method set, then size should be set as well"
	^ method size - size
]

{ #category : #testing }
CompiledMethodTrailer >> hasSource [
	^ kind == #EmbeddedSource or: [ kind == #EmbeddedSourceQCompress or: [ kind == #EmbeddedSourceZip ] ]
]

{ #category : #testing }
CompiledMethodTrailer >> hasSourcePointer [
	^ kind == #SourcePointer or: [ kind == #VarLengthSourcePointer ]
]

{ #category : #initialization }
CompiledMethodTrailer >> initialize [
	self clear
]

{ #category : #testing }
CompiledMethodTrailer >> isEmpty [
	^ kind == #NoTrailer or: [ kind == #ClearedTrailer ]
]

{ #category : #accessing }
CompiledMethodTrailer >> kind [
	"Answer a symbolic name of trailer kind.
	See #trailerKinds on class side and class comment for details"
	
	^ kind
]

{ #category : #private }
CompiledMethodTrailer >> kindAsByte [
	| index |
	index := self class trailerKinds indexOf: kind.
	[index ~~ 0] assert.

	^ (index - 1) << 2
]

{ #category : #accessing }
CompiledMethodTrailer >> method: aMethod [

	| flagByte |

	data := size := nil.
	method := aMethod.
	flagByte := method at: method size.

	"trailer kind encoded in 6 high bits of last byte"
	kind := self class trailerKinds at: 1+(flagByte>>2).

	"decode the trailer bytes, inline some common types to speed up decoding"
	kind == #SourcePointer
		ifTrue: [ ^self decodeSourcePointer ].
   kind == #VarLengthSourcePointer
		ifTrue: [ ^self decodeVarLengthSourcePointer ].
	kind == #EmbeddedSource
		ifTrue: [ ^self decodeEmbeddedSource ].
	kind == #NoTrailer
		ifTrue: [ ^self decodeNoTrailer ].

	"slow but general decoding using perform"
	self perform: ('decode' , kind) asSymbol.

]

{ #category : #private }
CompiledMethodTrailer >> qCompress: string [
	"A very simple text compression routine designed for method temp names.
	 Most common 11 chars get values 1-11 packed in one 4-bit nibble;
	 the next most common get values 12-15 (2 bits) * 16 plus next nibble;
	 unusual ones get three nibbles, the first being the escape nibble 0.

	Answer the write stream with compressed data inside"

	| utf8str stream oddNibble |

	string isEmpty ifTrue:
		[^self qCompress: ' '].
	utf8str := string convertToEncoding: 'utf8'.

	stream := WriteStream on: (ByteArray new: utf8str size).
	oddNibble := nil.

	utf8str do:	[:char | | ix |
		ix := 'ear tonsilcmbdfghjkpquvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345[]()'
			indexOf: char ifAbsent: 0.
		(ix = 0
			ifTrue:
				[{ 0. char asInteger // 16. char asInteger \\ 16 }]
			ifFalse:
				[ix <= 11
					ifTrue: [{ ix }]
					ifFalse: [{ ix//16+12. ix\\16 }]])
				do: [:nibble |
					oddNibble
						ifNotNil: [stream nextPut: oddNibble*16 + nibble. oddNibble := nil]
						ifNil: [oddNibble := nibble]]].
	oddNibble ifNotNil: "4 = 'ear tonsil' indexOf: Character space"
		[stream nextPut: oddNibble * 16 + 4].
	^ stream

]

{ #category : #accessing }
CompiledMethodTrailer >> size [
	"Answer the size of method's trailer , in bytes"
	^ size
]

{ #category : #accessing }
CompiledMethodTrailer >> sourceCode [
	"Answer the source code of compiled method as it was decoded.
	Note: it does not attempts to read from source files using sourcePointer,
	nor reconstruct the source code using temp names"
	
	kind == #EmbeddedSource ifTrue: [ ^data ].
	kind == #EmbeddedSourceWide ifTrue: [ ^data ].
	kind == #EmbeddedSourceQCompress ifTrue: [ ^data ].
	kind == #EmbeddedSourceZip ifTrue: [ ^data ].
	^ nil
]

{ #category : #accessing }
CompiledMethodTrailer >> sourceCode: aString [
	
	^ self embeddedSourceCode: aString
]

{ #category : #accessing }
CompiledMethodTrailer >> sourcePointer [

	kind == #NativeCodeTrailer ifTrue: [ ^ data at: 3].
	^ (kind == #SourcePointer or: [ kind == #VarLengthSourcePointer ] )
		ifTrue: [ data ]
		ifFalse: [ 0 ]

]

{ #category : #accessing }
CompiledMethodTrailer >> sourcePointer: ptr [

	self clear.
	data := ptr.
	"see if we can encode pointer using 4-byte trailer"
	kind := (ptr between: 16r1000000 and: 16r4FFFFFF) 
		ifTrue: [ #SourcePointer ] ifFalse: [ #VarLengthSourcePointer ].
	
]

{ #category : #testing }
CompiledMethodTrailer >> testEncoding [

	"Since we are using basic protocol (#at:, #at:put: , #size) for accessing compiled method data,
	we can pass the ByteArray instance into #method: accessor and check if encoding/decoding 
	operations	is symmetrical.
	Use this method only for unit-testing purposes"
	
	encodedData ifNil: [ self encode ].
	^ CompiledMethodTrailer new method: encodedData
]

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

{ #category : #kinds }
CompiledMethodTrailer class >> trailerKinds [
	" see class comment for description."

	^#(
"000000" #NoTrailer
"000001" #ClearedTrailer
"000010" #Undefined
"000011" #Undefined
"000100" #Undefined
"000101" #Undefined
"000110" #Undefined
"000111" #Undefined
"001000" #VarLengthSourcePointer
"001001" #Undefined
"001010" #Undefined
"001011" #Undefined
"001100" #Undefined
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
	data := 0.
	encodedData :=  #[0 0 0 0].
	method := nil
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

	self decodeLengthField
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
	size := 4
]

{ #category : #decoding }
CompiledMethodTrailer >> decodeSourcePointer [
	"Trailer is a source pointer"
	| msz |

	size := 4.
	msz := method size.
	data := (method at: msz) - 251 << 8 + (method at: msz-1) << 8
	 + (method at: msz-2) << 8 + (method at: msz-3)
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

	size := method size - pos
]

{ #category : #encoding }
CompiledMethodTrailer >> encode [
	"encode the trailer into byte array"

	encodedData := data asByteArrayOfSize: 4
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeClearedTrailer [

	"A cleared trailer is replaced by #NoTrailer, when used for encoding"
	self clear.
	kind := #NoTrailer.

	^ self encode
]

{ #category : #private }
CompiledMethodTrailer >> encodeLengthField: integer [
	| bytes value |
	[integer >= 0] assert.
	integer = 0 ifTrue: [ ^#[0] ].
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
	encodedData := #[0 0 0 0]
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeSourcePointer [

	encodedData := ByteArray new: 4.
	encodedData at: 4 put: (data >> 24) + 251.
	1 to: 3 do: [:i |
		encodedData at: 4-i put: ((data bitShift: (i-3)*8) bitAnd: 16rFF)].
	size := encodedData size
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeUndefined [

	self error: 'use of an undefined kind of trailer encoding'
]

{ #category : #encoding }
CompiledMethodTrailer >> encodeVarLengthSourcePointer [

	"source pointer must be >=0"
	"[data >= 0] assert."

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
	encodedData := encodedData reversed copyWith: (self kindAsByte).
	size := encodedData size
]

{ #category : #accessing }
CompiledMethodTrailer >> encodedData [
	^encodedData
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
	"Not supported anymore, trailers never encode source"
	^ false
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

		| msz |
	method := aMethod.
	kind := #SourcePointer.
	msz := method size.
	encodedData := {
		method at: msz-3.
		method at: msz-2.
		method at: msz-1.
		method at: msz } asByteArray.
	data := encodedData asInteger
]

{ #category : #accessing }
CompiledMethodTrailer >> size [
	"Answer the size of method's trailer , in bytes"
	^ size
]

{ #category : #accessing }
CompiledMethodTrailer >> sourceCode [
	"Not supported anymore, trailers never encode source"
	^ nil
]

{ #category : #accessing }
CompiledMethodTrailer >> sourcePointer [

	^ (kind == #SourcePointer or: [ kind == #VarLengthSourcePointer ] )
		ifTrue: [ data ]
		ifFalse: [ 0 ]
]

{ #category : #accessing }
CompiledMethodTrailer >> sourcePointer: ptr [

	self clear.
	data := ptr.
	"see if we can encode pointer using 4-byte trailer"
	kind := #SourcePointer
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

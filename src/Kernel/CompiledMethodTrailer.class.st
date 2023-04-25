"
I am responsible for encoding and decoding various kinds of compiled method trailer data.

I should not expose any binary data outside of myself, so all tools which working with compiled methods
should ask me to encode the meta-data, they want to be added to the compiled method trailer, as well as retrieve it.

Trailers are now very simple: a fixed number of bytes (currently 4, this will be increased later to support .changes >2GB).

"
Class {
	#name : #CompiledMethodTrailer,
	#superclass : #Object,
	#instVars : [
		'data',
		'encodedData',
		'kind',
		'method'
	],
	#category : #'Kernel-Methods'
}

{ #category : #kinds }
CompiledMethodTrailer class >> empty [
	"answer the empty trailer"
	^ self new
]

{ #category : #initialization }
CompiledMethodTrailer >> clear [
	kind := #NoTrailer.
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

	meth := aCompiledMethodClass newMethod: numberOfBytesForAllButTrailer + encodedData size header: headerWord.
	"copy the encoded trailer data"
	1 to: encodedData size do:
		[:i | meth at: meth size - encodedData size + i put: (encodedData at: i)].

	^meth
]

{ #category : #encoding }
CompiledMethodTrailer >> encode [
	"encode the trailer into byte array"

	encodedData := data asByteArrayOfSize: 4
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
	^ method size - encodedData size
]

{ #category : #testing }
CompiledMethodTrailer >> hasSource [
	"Not supported anymore, trailers never encode source"
	^ false
]

{ #category : #testing }
CompiledMethodTrailer >> hasSourcePointer [
	encodedData = #[0 0 0 0] ifTrue: [ ^false ].
	^ kind == #SourcePointer
]

{ #category : #initialization }
CompiledMethodTrailer >> initialize [
	self clear
]

{ #category : #testing }
CompiledMethodTrailer >> isEmpty [
	^ kind == #NoTrailer or: [ kind == #ClearedTrailer or: [encodedData = #[0 0 0 0]]]
]

{ #category : #accessing }
CompiledMethodTrailer >> kind [
	"Answer a symbolic name of trailer kind.
	See #trailerKinds on class side and class comment for details"

	^ kind
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
	^ encodedData size
]

{ #category : #accessing }
CompiledMethodTrailer >> sourceCode [
	"Not supported anymore, trailers never encode source"
	^ nil
]

{ #category : #accessing }
CompiledMethodTrailer >> sourcePointer [

	^ kind == #SourcePointer
		ifTrue: [ data ]
		ifFalse: [ 0 ]
]

{ #category : #accessing }
CompiledMethodTrailer >> sourcePointer: ptr [

	data := ptr.
	kind := #SourcePointer.
	self encode.
	
]

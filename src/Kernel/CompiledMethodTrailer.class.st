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
		'method'
	],
	#category : #'Kernel-Methods'
}

{ #category : #kinds }
CompiledMethodTrailer class >> empty [
	"answer the empty trailer"
	^ self new
]

{ #category : #'creating a method' }
CompiledMethodTrailer >> createMethod: numberOfBytesForAllButTrailer class: aCompiledMethodClass header: headerWord [
	| meth size |
	"Instantiates a compiled method with this trailer.
	- numberOfBytesForAllButTrailer specifies the number of bytes needed to encode the literals and the bytecode.
	- aCompiledMethodClass specifies the concrete class of the compiled method.
	- headerWord specifies the header used for the compiled method.

	This method will create a compiled method instance of the correct size, set it's header and trailing bytes. The bytecode and literals must be set by the caller."
	size := CompiledMethod trailerSize.
	encodedData := data asByteArrayOfSize: size.
	
	meth := aCompiledMethodClass newMethod: numberOfBytesForAllButTrailer + size header: headerWord.
	"copy the encoded trailer data"
	1 to: size do:
		[:i | meth at: meth size - size + i put: (encodedData at: i)].

	^meth
]

{ #category : #testing }
CompiledMethodTrailer >> hasSourcePointer [

	^ encodedData ~= #[ 0 0 0 0 ]
]

{ #category : #initialization }
CompiledMethodTrailer >> initialize [
	data := 0.
	encodedData :=  #[0 0 0 0]
]

{ #category : #accessing }
CompiledMethodTrailer >> method: aMethod [

	| msz |
	method := aMethod.
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
	^ CompiledMethod trailerSize
]

{ #category : #accessing }
CompiledMethodTrailer >> sourceCode [
	"Not supported anymore, trailers never encode source"
	^ nil
]

{ #category : #accessing }
CompiledMethodTrailer >> sourcePointer [

	^ data
]

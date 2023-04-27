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
CompiledMethodTrailer >> sourcePointer [

	^ data
]

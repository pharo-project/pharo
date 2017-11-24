"
I am ZnHttpRetrieveContents.
I am a ZnUrlOperation.

I implement the #retrieveContents URL operation for HTTP(S) urls.

  'http://zn.stfx.eu/zn/numbers.txt' asZnUrl retrieveContents.

Part of Zinc HTTP Components.

"
Class {
	#name : #ZnHttpRetrieveContents,
	#superclass : #ZnUrlOperation,
	#category : #'Zinc-HTTP-Client-Server'
}

{ #category : #accessing }
ZnHttpRetrieveContents class >> operation [
	^ #retrieveContents
]

{ #category : #accessing }
ZnHttpRetrieveContents class >> schemes [
	^ #( #http #https )
]

{ #category : #operations }
ZnHttpRetrieveContents >> performOperation [
	"Download and return the HTTP(S) resource that url refers to.
	This will typically return a String or a ByteArray (see ZnClient>>#get:).
	If something goes wrong, an exception will be signalled."

	^ ZnClient new
		beOneShot;
		enforceHttpSuccess: true;
		get: self url
]

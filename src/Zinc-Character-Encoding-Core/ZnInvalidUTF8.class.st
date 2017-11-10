"
I am ZnInvalidUTF8.
I am a ZnCharacterEncodingError.
I am an Error.

I signal when something goes wrong while encoding or decoding UTF8.

I can be used to ignore wrongly encoded input by resuming me. By default a question mark will be inserted for each problem and decoding will continue. This is not recommended, as faulty input should not be accepted.

Part of Zinc HTTP Components
"
Class {
	#name : #ZnInvalidUTF8,
	#superclass : #ZnCharacterEncodingError,
	#category : #Zinc-Character-Encoding-Core
}

{ #category : #private }
ZnInvalidUTF8 >> defaultResumeValue [
	"$? codePoint"
	
	^ 63
]

{ #category : #testing }
ZnInvalidUTF8 >> isResumable [
	^ true
]

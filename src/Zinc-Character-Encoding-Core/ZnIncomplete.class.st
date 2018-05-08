"
I am ZnIncomplete.
I am a ZnCharacterEncodingError.
I am an Error.

I signal when the binary stream from which a character is read does not contain enough data to form a full character. This typically occurs when the stream is #atEnd, a file is EOF or a network connection is closed - when the end of a stream is reached when more data is expected/needed.

I can be used to ignore wrongly encoded input by resuming me. By default a question mark will be inserted for each problem and decoding will continue. This is not recommended, as faulty input should not be accepted.

Part of Zinc HTTP Components
"
Class {
	#name : #ZnIncomplete,
	#superclass : #ZnCharacterEncodingError,
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #private }
ZnIncomplete >> defaultResumeValue [
	"$? codePoint"
	
	^ 63
]

{ #category : #testing }
ZnIncomplete >> isResumable [
	^ true
]

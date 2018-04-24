"
This is an Error that knows how to get the Freetype2 error code and string.
"
Class {
	#name : #FT2Error,
	#superclass : #Error,
	#instVars : [
		'errorCode',
		'errorString'
	],
	#category : #'FreeType-Base'
}

{ #category : #'instance creation' }
FT2Error class >> errorCode: code errorString: str signal: message [
	^ self new
		messageText: message;
		errorCode: code;
		errorString: str;
		signal
]

{ #category : #'instance creation' }
FT2Error class >> errorCode: code signal: message [
	^ self new
		messageText: message;
		errorCode: code;
		errorString: (FT2FFILibrary translateErrorCode: code);
		signal
]

{ #category : #accessing }
FT2Error >> errorCode [
	^ errorCode

]

{ #category : #accessing }
FT2Error >> errorCode: anObject [
	errorCode := anObject
]

{ #category : #accessing }
FT2Error >> errorString [
	^ errorString

]

{ #category : #accessing }
FT2Error >> errorString: anObject [
	errorString := anObject
]

{ #category : #accessing }
FT2Error >> messageText [
	^String streamContents: [ :strm |
		messageText ifNotNil: [ strm nextPutAll: messageText; space ].
		self errorCode isZero ifFalse: [
			strm nextPutAll: '[error '; print: self errorCode; nextPutAll: '][';
				nextPutAll: self errorString;
				nextPut: $] ]]
]

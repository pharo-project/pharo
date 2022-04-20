"
This class absorb the difference of internal and external representation of the file path.  The idea is to keep the internal one as much as possible, and only when it goes to a primitive, the encoded file path, i.e. the native platform representation is passsed to the primitive.

	The converter used is obtained by ""LanguageEnvironment defaultFileNameConverter"".

"
Class {
	#name : #FilePathEncoder,
	#superclass : #Object,
	#category : #Deprecated11
}

{ #category : #encoding }
FilePathEncoder class >> decode: aString [

	self 
		deprecated: 'Please use #utf8Decoded directly' 
		transformWith:  '`@receiver decode: `@arg' 
						-> '`@arg utf8Decoded'.
						
	^ aString utf8Decoded
]

{ #category : #encoding }
FilePathEncoder class >> encode: pathString [

	self 
		deprecated: 'Please use #utf8Encoded directly' 
		transformWith:  '`@receiver encode: `@arg' 
						-> '`@arg utf8Encoded'.

	^ pathString utf8Encoded
]

{ #category : #testing }
FilePathEncoder class >> isDeprecated [ 

	^true
]

Extension { #name : #String }

{ #category : #'*Deprecated90' }
String >> convertFromEncoding: encodingName [

	self 
        deprecated: 'Use #utf8Decoded or #decodeWith: encoding on a byte array. Using a string is not valid. You should have a byte array as input'
        on: '2021-08-27'
        in: #Pharo10.

	^ self asByteArray decodeWith: encodingName
]

{ #category : #'*Deprecated90' }
String >> convertFromWithConverter: converter [

	| readStream c |
	readStream := self readStream.
	^ self species new: self size streamContents: [ :writeStream|
		converter ifNil: [^ self].
		[readStream atEnd] whileFalse: [
			c := converter nextFromStream: readStream.
			c
				ifNotNil: [writeStream nextPut: c] 
				ifNil: [^ writeStream contents]]].

]

{ #category : #'*Deprecated90' }
String >> convertToEncoding: encodingName [

	self
        deprecated: 'Use #utf8Encoded or #encodeWith: .'
        on: '2021-08-27'
        in: #Pharo10.

	^ self encodeWith: encodingName
]

{ #category : #'*Deprecated90' }
String >> convertToWithConverter: converter [ 
	converter
		ifNil: [^ self].
	^ String new: self size streamContents: [:writeStream | 
		converter 
			nextPutAll: self toStream: writeStream;
			emitSequenceToResetStateIfNeededOn: writeStream]
]

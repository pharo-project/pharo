Extension { #name : #String }

{ #category : #'*Multilingual-TextConversion' }
String >> convertFromEncoding: encodingName [
	^self convertFromWithConverter: (TextConverter newForEncoding: encodingName)
]

{ #category : #'*Multilingual-TextConversion' }
String >> convertFromWithConverter: converter [

	| readStream c |
	readStream := self readStream.
	^ self class new: self size streamContents: [ :writeStream|
		converter ifNil: [^ self].
		[readStream atEnd] whileFalse: [
			c := converter nextFromStream: readStream.
			c
				ifNotNil: [writeStream nextPut: c] 
				ifNil: [^ writeStream contents]]].

]

{ #category : #'*Multilingual-TextConversion' }
String >> convertToEncoding: encodingName [
	^self convertToWithConverter: (TextConverter newForEncoding: encodingName).
]

{ #category : #'*Multilingual-TextConversion' }
String >> convertToWithConverter: converter [ 
	converter
		ifNil: [^ self].
	^ String new: self size streamContents: [:writeStream | 
		converter 
			nextPutAll: self toStream: writeStream;
			emitSequenceToResetStateIfNeededOn: writeStream]
]

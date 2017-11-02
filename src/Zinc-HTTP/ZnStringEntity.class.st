"
I am ZnStringEntity, a concrete HTTP Entity based on a String.
It is used to hold textual (non-binary) data.
I am a ZnEntity.

Optionally, an encoding is used to convert to and from bytes.
The default encoding it UTF-8.

Note that content length is the encoded byte count, not the number of characters in the string.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnStringEntity,
	#superclass : #ZnEntity,
	#instVars : [
		'string',
		'encoder'
	],
	#category : 'Zinc-HTTP-Core'
}

{ #category : #testing }
ZnStringEntity class >> designatedMimeType [
	"I have no designated mime type, I can handle all non-binary content.
	See #concreteSubclassForType: "
	
	^ nil
]

{ #category : #'instance creation' }
ZnStringEntity class >> html: string [
	^ (self type: ZnMimeType textHtml)
		string: string;
		yourself
]

{ #category : #'instance creation' }
ZnStringEntity class >> json: string [
	^ (self type: ZnMimeType applicationJson)
		string: string;
		yourself
]

{ #category : #testing }
ZnStringEntity class >> matches: mimeType [
	"I have no designated mime type, I can handle all non-binary content.
	See #concreteSubclassForType: "
	
	^ false
]

{ #category : #'instance creation' }
ZnStringEntity class >> text: string [
	^ (self type: ZnMimeType textPlain)
		string: string;
		yourself
]

{ #category : #'instance creation' }
ZnStringEntity class >> xml: string [
	^ (self type: ZnMimeType applicationXml)
		string: string;
		yourself
]

{ #category : #comparing }
ZnStringEntity >> = other [
	^ super = other and: [ self string = other string ]
]

{ #category : #private }
ZnStringEntity >> computeContentLength [
	self string isNil ifTrue: [ ^ self ].
	contentLength := self encoder encodedByteCountForString: string
]

{ #category : #accessing }
ZnStringEntity >> contentLength [
	contentLength isNil ifTrue: [ self computeContentLength ].
	^ contentLength
]

{ #category : #accessing }
ZnStringEntity >> contentType: object [
	"We only allow assignment compatible with our designated mime type"
	
	| newType |
	newType := object asZnMimeType.
	contentType = newType 
		ifTrue: [ ^ self ]
		ifFalse: [ 
			newType isBinary
				ifFalse: [ 
					contentType := newType.
					self invalidateEncoder ] ]
]

{ #category : #accessing }
ZnStringEntity >> contents [
	^ self string
]

{ #category : #accessing }
ZnStringEntity >> encoder [
	self initializeEncoder.
	^ encoder
]

{ #category : #accessing }
ZnStringEntity >> encoder: anEncoding [
	(encoder notNil and: [ anEncoding ~= encoder ]) ifTrue: [ self invalidateContentLength ].
	encoder := anEncoding
]

{ #category : #private }
ZnStringEntity >> hasEncoder [
	^ encoder notNil
]

{ #category : #comparing }
ZnStringEntity >> hash [
	^ super hash bitXor: self string hash
]

{ #category : #private }
ZnStringEntity >> initializeEncoder [
	"If the content-type specifies an encoding, use that.
	If not, use the value of ZnDefaultCharacterEncoder,
	if that is not set, fall back to UTF-8"
	
	| charSet newEncoder |
	self hasEncoder
		ifTrue: [ ^ self ].
	newEncoder := (charSet := contentType charSet) isNil
		ifTrue: [ ZnDefaultCharacterEncoder value ]
		ifFalse: [ 
			(ZnCharacterEncoder newForEncoding: charSet)
				beLenient;
				yourself ].
	self encoder: newEncoder
]

{ #category : #private }
ZnStringEntity >> invalidateContentLength [
	self contentLength: nil
]

{ #category : #private }
ZnStringEntity >> invalidateEncoder [
	self encoder: nil
]

{ #category : #testing }
ZnStringEntity >> isEmpty [ 
	^ self string isNil or: [ self string isEmpty ] 
]

{ #category : #printing }
ZnStringEntity >> printContentsOn: stream [
	super printContentsOn: stream.
	self string notNil ifTrue: [ 
		stream space; nextPutAll: self string ]
]

{ #category : #initialize-release }
ZnStringEntity >> readFrom: stream [
	| buffer totalRead read readStream stringStream total |
	total := self contentLength.
	readStream := total ifNotNil: [ ZnLimitedReadStream on: stream limit: total ] ifNil: [ stream ].
	buffer := String new: (ZnUtils streamingBufferSize min: (total ifNil: [ ZnUtils streamingBufferSize ])).
	stringStream := nil.
	totalRead := 0.
	self initializeEncoder.
	[ readStream atEnd ] whileFalse: [
		[ read := encoder readInto: buffer startingAt: 1 count: buffer size fromStream: readStream ]
			on: ZnByteStringBecameWideString 
			do: [ :notification | 
					buffer := notification wideString.
					stringStream ifNotNil: [ | wideString position |
						position := stringStream position.
						wideString := WideString from: stringStream originalContents.
						stringStream on: wideString; setFrom: position + 1 to: position ].
					notification resume ].
		totalRead := totalRead + read.
		totalRead > ZnMaximumEntitySize value
			ifTrue: [ ZnEntityTooLarge signal ].
		stringStream ifNil: [ 
			readStream atEnd
				ifTrue: [ ^ self string: (buffer copyFrom: 1 to: read) ]
				ifFalse: [ stringStream := (total ifNil: [ buffer species new ] ifNotNil: [ buffer species new: total ]) writeStream ] ].
		stringStream next: read putAll: buffer startingAt: 1.
		ZnUtils signalProgress: totalRead total: total ].
	self string: (stringStream ifNil: [ String new ] ifNotNil: [ stringStream contents ])
]

{ #category : #accessing }
ZnStringEntity >> readStream [
	^ self string readStream
]

{ #category : #accessing }
ZnStringEntity >> string [
	^ string
]

{ #category : #accessing }
ZnStringEntity >> string: aString [
	(string notNil and: [ aString ~= string ]) ifTrue: [ self invalidateContentLength ].
	string := aString
]

{ #category : #writing }
ZnStringEntity >> writeOn: stream [
	| totalWritten toWrite total  |
	self initializeEncoder.
	totalWritten := 0.
	total := string size.
	[ totalWritten < total ] whileTrue: [ 
		toWrite := ZnUtils streamingBufferSize min: (total - totalWritten).
		encoder next: toWrite putAll: string startingAt: totalWritten + 1 toStream: stream.
		totalWritten := totalWritten + toWrite.
		ZnUtils signalProgress: totalWritten total: total ]
]

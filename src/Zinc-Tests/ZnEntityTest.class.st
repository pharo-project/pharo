Class {
	#name : #ZnEntityTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnEntityTest >> testApplicationUrlEncodingAddAll [
	| entity string data |
	data := Dictionary new.
	data at: 'foo' put: 1 asString.
	data at: 'bar' put: 2 asString.
	entity := ZnApplicationFormUrlEncodedEntity new.
	"Asking for the content length will force an internal computation of the representation"
	self assert: entity contentLength equals: 0.
	entity addAll: data.
	string := String streamContents: [ :stream | entity writeOn: stream ].
	self assert: entity contentLength > 0.
	entity := ZnEntity readFrom: string readStream usingType: ZnApplicationFormUrlEncodedEntity designatedMimeType andLength: string size.
	self assert: (entity at: 'foo') equals: '1'.
	self assert: (entity at: 'bar') equals: '2'
]

{ #category : #testing }
ZnEntityTest >> testMultiPartFormDataWriteRead [
	| input output bytes |
	input := ZnMultiPartFormDataEntity new
		addPart: (ZnMimePart fieldName: 'extra' value: 'my-extra');
		addPart: (ZnMimePart fieldName: 'info' value: 'my-info');
		addPart: (ZnMimePart fieldName: 'file' fileName: 'foo.txt' entity: (ZnEntity text: 'Zinc HTTP Components'));
		yourself.
	self assert: input contentLength notNil.
	self assert: input contentLength > 0.
	self assert: (input contentType matches: ZnMimeType multiPartFormData).
	bytes := ByteArray streamContents: [ :stream | input writeOn: stream ].
	output := (ZnMultiPartFormDataEntity type: input contentType) readFrom: bytes readStream.
	self assert: (output partNamed: 'extra') fieldValue equals: 'my-extra'.
	self assert: (output partNamed: 'info') fieldValue equals: 'my-info'.
	self assert: (output partNamed: 'extra') fieldValueString equals: 'my-extra'.
	self assert: (output partNamed: 'info') fieldValueString equals: 'my-info'.
	self assert: (output partNamed: 'file') contents equals: 'Zinc HTTP Components'.
	output := (ZnMultiPartFormDataEntity type: input contentType length: input contentLength) readFrom: bytes readStream.
	self assert: (output partNamed: 'extra') fieldValue equals: 'my-extra'.
	self assert: (output partNamed: 'info') fieldValue equals: 'my-info'.
	self assert: (output partNamed: 'extra') fieldValueString equals: 'my-extra'.
	self assert: (output partNamed: 'info') fieldValueString equals: 'my-info'.
	self assert: (output partNamed: 'file') contents equals: 'Zinc HTTP Components'
]

{ #category : #testing }
ZnEntityTest >> testMultiPartFormDataWriteReadBinary [
	| input output bytes |
	input := ZnMultiPartFormDataEntity new
		addPart: (ZnMimePart fieldName: 'extra' value: 'my-extra');
		addPart: (ZnMimePart fieldName: 'info' value: 'my-info');
		addPart: (ZnMimePart fieldName: 'file' fileName: 'foo.txt' entity: (ZnEntity text: 'Zinc HTTP Components'));
		yourself.
	self assert: input contentLength notNil.
	self assert: input contentLength > 0.
	self assert: (input contentType matches: ZnMimeType multiPartFormData).
	bytes := ByteArray streamContents: [ :stream | input writeOn: stream ].
	output := ZnEntity readBinaryFrom: bytes readStream usingType: input contentType andLength: bytes size.
	self assert: (output partNamed: 'extra') fieldValue equals: 'my-extra' asByteArray.
	self assert: (output partNamed: 'info') fieldValue equals: 'my-info' asByteArray.
	self assert: (output partNamed: 'extra') fieldValueString equals: 'my-extra'.
	self assert: (output partNamed: 'info') fieldValueString equals: 'my-info'.
	self deny: (output partNamed: 'file') contents isString.
	self assert: (output partNamed: 'file') contents equals: 'Zinc HTTP Components' asByteArray
]

{ #category : #testing }
ZnEntityTest >> testReading [
	| contents entity |
	contents := 'This is a test'.
	entity := ZnEntity readFrom: contents asByteArray readStream usingType: ZnMimeType textPlain andLength: contents size.
	self assert: entity contents isString.
	self assert: entity string equals: contents.
	self assert: entity contentLength equals: contents size.
	self assert: entity contentType equals: ZnMimeType textPlain
]

{ #category : #testing }
ZnEntityTest >> testReadingApplicationFormUrlEncoding [
	| input entity |
	input := #(111 97 117 116 104 95 116 111 107 101 110 95 115 101 99 114 101 116 61 98 121 112 104 57 99 109 104 106 49 53 101 108 121 56 38 111 97 117 116 104 95 116 111 107 101 110 61 56 54 101 112 106 51 116 118 100 49 107 115 120 111 57)
		asByteArray.
	entity := ZnEntity readFrom: input readStream usingType: ZnMimeType applicationFormUrlEncoded andLength: input size.
	self deny: entity isEmpty.
	self assert: (entity includesField: 'oauth_token_secret').
	self assert: (entity includesField: 'oauth_token').
	self assert: (entity at: 'oauth_token_secret') equals: 'byph9cmhj15ely8'.
	self assert: (entity at: 'oauth_token') equals: '86epj3tvd1ksxo9'
]

{ #category : #testing }
ZnEntityTest >> testReadingApplicationFormUrlEncodingNoLength [
	| input entity |
	input := #(111 97 117 116 104 95 116 111 107 101 110 95 115 101 99 114 101 116 61 98 121 112 104 57 99 109 104 106 49 53 101 108 121 56 38 111 97 117 116 104 95 116 111 107 101 110 61 56 54 101 112 106 51 116 118 100 49 107 115 120 111 57)
		asByteArray.
	entity := ZnEntity readFrom: input readStream usingType: ZnMimeType applicationFormUrlEncoded andLength: nil.
	self deny: entity isEmpty.
	self assert: (entity includesField: 'oauth_token_secret').
	self assert: (entity includesField: 'oauth_token').
	self assert: (entity at: 'oauth_token_secret') equals: 'byph9cmhj15ely8'.
	self assert: (entity at: 'oauth_token') equals: '86epj3tvd1ksxo9'
]

{ #category : #testing }
ZnEntityTest >> testReadingBinary [
	| contents entity |
	contents := 'This is a test'.
	entity := ZnEntity readBinaryFrom: contents asByteArray readStream usingType: ZnMimeType textPlain andLength: contents size.
	self deny: entity contents isString.
	self assert: entity contents equals: contents asByteArray.
	self assert: entity contentLength equals: contents size.
	self assert: entity contentType equals: ZnMimeType textPlain
]

{ #category : #testing }
ZnEntityTest >> testReadingLimited [
	| contents |
	contents := String new: 1025 withAll: $X.
	ZnMaximumEntitySize value: 1024 during: [
		self should: [
			ZnEntity 
				readFrom: contents asByteArray readStream 
				usingType: ZnMimeType textPlain 
				andLength: contents size ]
			raise: ZnEntityTooLarge.
		self should: [
			ZnEntity 
				readFrom: contents asByteArray readStream 
				usingType: ZnMimeType textPlain 
				andLength: nil ]
			raise: ZnEntityTooLarge ] 
]

{ #category : #testing }
ZnEntityTest >> testStringEntityEncoderInitialization [
	| entity response |
	entity := ZnEntity text: 'Regression test'.
	self assert: entity hasEncoder not.
	response := ZnResponse ok: entity.
	self assert: entity hasEncoder.
	ByteArray streamContents: [ :stream | response writeOn: stream ].
	self assert: entity hasEncoder
	
]

{ #category : #testing }
ZnEntityTest >> testUTF8ReadingDetermined [
	| string entity bytes |
	string := String
		with: $$
		with: 16r00A2 asCharacter
		with: 16r20AC asCharacter
		with: 16r024B62 asCharacter.
	entity := ZnStringEntity type: ZnMimeType textPlain length: 10.
	bytes := #(16r24 16rC2 16rA2 16rE2 16r82 16rAC 16rF0 16rA4 16rAD 16rA2) asByteArray.
	entity readFrom: bytes readStream.
	self assert: entity contentType equals: ZnMimeType textPlain.
	self assert: entity contentLength equals: 10.
	self assert: entity contents equals: string
]

{ #category : #testing }
ZnEntityTest >> testUTF8ReadingUndetermined [
	| string entity bytes |
	string := String
		with: $$
		with: 16r00A2 asCharacter
		with: 16r20AC asCharacter
		with: 16r024B62 asCharacter.
	entity := ZnStringEntity type: ZnMimeType textPlain.
	bytes := #(16r24 16rC2 16rA2 16rE2 16r82 16rAC 16rF0 16rA4 16rAD 16rA2) asByteArray.
	entity readFrom: bytes readStream.
	self assert: entity contentType equals: ZnMimeType textPlain.
	self assert: entity contentLength equals: 10.
	self assert: entity contents equals: string
]

{ #category : #testing }
ZnEntityTest >> testUTF8Writing [
	| string entity bytes |
	string := String
		with: $$
		with: 16r00A2 asCharacter
		with: 16r20AC asCharacter
		with: 16r024B62 asCharacter.
	entity := ZnStringEntity text: string.
	self assert: entity contentType equals: ZnMimeType textPlain.
	self assert: entity contentLength equals: 10.
	bytes := ByteArray streamContents: [ :stream | entity writeOn: stream ].
	self assert: bytes equals: #(16r24 16rC2 16rA2 16rE2 16r82 16rAC 16rF0 16rA4 16rAD 16rA2) asByteArray
]

{ #category : #testing }
ZnEntityTest >> testUnspecifiedEncoding [
	| json bytes entity |
	json := '{"text":"Der Weg zur Hölle ist mit guten Vorsätzen gepflastert."}'.
	entity := ZnEntity with: json type: ZnMimeType applicationJson setCharSetUTF8.
	bytes := entity asByteArray.
	ZnDefaultCharacterEncoder 
		value: ZnUTF8Encoder new
		during: [
			entity := ZnEntity 
				readFrom: bytes readStream 
				usingType: ZnMimeType applicationJson 
				andLength: nil. 
			self assert: entity contents equals: json.
			entity := ZnEntity with: json type: ZnMimeType applicationJson.
			self assert: entity encoder equals: ZnUTF8Encoder new ]
]

{ #category : #testing }
ZnEntityTest >> testWritingApplicationUrlEncoding [
	| entity string |
	entity := ZnApplicationFormUrlEncodedEntity new.
	entity at: 'foo' put: 'bar'.
	string := String streamContents: [ :stream | entity writeOn: stream ].
	self assert: string equals: 'foo=bar'.
	self assert: entity contentLength equals: 7
]

{ #category : #testing }
ZnEntityTest >> testWritingApplicationUrlEncodingWithTextEncodingLatin1 [
	| entity string |
	entity := ZnApplicationFormUrlEncodedEntity new.
	entity contentType charSet: 'iso-8859-1'.
	entity at: 'foo' put: (Character value: 246) asString.
	string := String streamContents: [ :stream | entity writeOn: stream ].
	self assert: string equals: 'foo=%F6'.
	self assert: entity contentLength equals: 7
]

{ #category : #testing }
ZnEntityTest >> testWritingApplicationUrlEncodingWithTextEncodingUtf8 [
	| entity string |
	entity := ZnApplicationFormUrlEncodedEntity new.
	entity contentType charSet: 'utf-8'.
	entity at: 'foo' put: (Character value: 246) asString.
	string := String streamContents: [ :stream | entity writeOn: stream ].
	self assert: string equals: 'foo=%C3%B6'.
	self assert: entity contentLength equals: 10
]

Class {
	#name : 'ZnDataUrl',
	#superclass : 'Object',
	#instVars : [
		'mediaType',
		'data',
		'base64Encoded'
	],
	#category : 'Zinc-Resource-Meta-Core',
	#package : 'Zinc-Resource-Meta-Core'
}

{ #category : 'instance creation' }
ZnDataUrl class >> bytes: bytes type: mimeType [
	"self bytes: #[71 73 70 56 55 97 48 0 48 0 240 0 0 0 0 0 255 255 255 44 0 0 0 0 48 0 48 0 0 2 240 140 143 169 203 237 223 0 156 14 72 139 115 176 180 171 12 134 30 20 150 166 52 46 231 42 166 9 139 26 167 43 175 81 73 111 56 217 142 102 215 243 128 92 193 201 48 117 209 8 49 57 29 19 168 20 30 142 20 77 204 231 228 84 159 185 98 37 42 173 113 121 99 153 74 135 240 222 184 215 15 231 34 214 26 193 27 180 184 142 74 150 191 76 248 59 38 146 71 199 39 167 119 53 147 5 245 244 115 19 167 40 248 224 7 56 184 118 40 167 88 103 100 23 201 35 117 249 242 113 6 87 101 230 6 122 57 137 149 151 134 151 216 182 137 197 106 104 213 90 138 84 250 23 152 137 247 73 186 154 251 147 75 131 138 211 147 52 188 57 52 67 133 212 183 11 202 59 58 220 119 120 170 246 232 172 23 205 76 212 226 28 71 185 68 123 12 28 46 158 237 184 211 100 51 123 141 157 173 197 216 148 5 163 222 213 174 44 191 94 94 63 127 87 31 229 85 179 95 44 166 236 241 225 135 30 29 110 6 10 0 0 59] type: ZnMimeType imageGif"
	
	^ self new
		mediaType: mimeType;
		data: bytes;
		base64Encoded: true;
		yourself
]

{ #category : 'instance creation' }
ZnDataUrl class >> fromString: string [
	"Parse the external representation of a Data URI from string.
	The general format being data:[<mediatype>][;base64],<data>"
	
	"self fromString: 'data:image/gif;base64,R0lGODdhMAAwAPAAAAAAAP///ywAAAAAMAAwAAAC8IyPqcvt3wCcDkiLc7C0qwyGHhSWpjQu5yqmCYsapyuvUUlvONmOZtfzgFzByTB10QgxOR0TqBQejhRNzOfkVJ+5YiUqrXF5Y5lKh/DeuNcP5yLWGsEbtLiOSpa/TPg7JpJHxyendzWTBfX0cxOnKPjgBzi4diinWGdkF8kjdfnycQZXZeYGejmJlZeGl9i2icVqaNVailT6F5iJ90m6mvuTS4OK05M0vDk0Q4XUtwvKOzrcd3iq9uisF81M1OIcR7lEewwcLp7tuNNkM3uNna3F2JQFo97Vriy/Xl4/f1cf5VWzXyym7PHhhx4dbgYKAAA7'"
	
	^ self new
		parseFrom: string;
		yourself
]

{ #category : 'instance creation' }
ZnDataUrl class >> text: string [
	"self text: 'Hello there !'"
	
	^ self new
		mediaType: ZnMimeType textPlain;
		data: string;
		yourself
]

{ #category : 'comparing' }
ZnDataUrl >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ mediaType = anObject mediaType and: [
		  base64Encoded = anObject isBase64Encoded and: [
			  data = anObject data ] ]
]

{ #category : 'initialization' }
ZnDataUrl >> base64Encoded: boolean [
	base64Encoded := boolean
]

{ #category : 'accessing' }
ZnDataUrl >> data [
	"Answer the data that I hold, either a ByteArray or a String"
	
	^ data
]

{ #category : 'initialization' }
ZnDataUrl >> data: stringOrBytes [
	data := stringOrBytes
]

{ #category : 'comparing' }
ZnDataUrl >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ mediaType hash bitXor: (base64Encoded hash bitXor: data hash)
]

{ #category : 'initialization' }
ZnDataUrl >> initialize [
	super initialize.
	mediaType := 'text/plain;charset=ASCII' asZnMimeType.
	base64Encoded := false.
	data := #[]
]

{ #category : 'testing' }
ZnDataUrl >> isBase64Encoded [
	"Am I using Base64 encoding ?"
	
	^ base64Encoded
]

{ #category : 'testing' }
ZnDataUrl >> isBinary [
	"Am I binary ?"
	
	^ mediaType isBinary
]

{ #category : 'testing' }
ZnDataUrl >> isEmpty [
	^ data isEmpty
]

{ #category : 'accessing' }
ZnDataUrl >> mediaType [
	"Answer the media type that described my content, a ZnMimeType"
	
	^ mediaType
]

{ #category : 'initialization' }
ZnDataUrl >> mediaType: mimeType [
	mediaType := mimeType asZnMimeType
]

{ #category : 'parsing' }
ZnDataUrl >> parseFrom: string [
	| commaIndex |
	(string beginsWith: 'data:')
		ifFalse: [ ^ self error: 'missing data: scheme' ].
	(commaIndex := string indexOf: $,) isZero
		ifTrue: [ ^ self error: 'missing comma in data: scheme' ].
	commaIndex = string size 
		ifTrue: [ ^ self ].
	commaIndex > 7
		ifTrue: [ mediaType := ZnMimeType fromString: (string copyFrom: 6 to: commaIndex - 1) ].
	(base64Encoded := mediaType parameters includesKey: 'base64')
		ifTrue: [ 
			 mediaType removeParameter: 'base64' ].
	data := string copyFrom: commaIndex + 1 to: string size.
	data := base64Encoded 
		ifTrue: [ 
			ZnBase64Encoder new
				decode: data ]
		ifFalse: [
			ZnPercentEncoder new 
				characterEncoder: mediaType charSet asZnCharacterEncoder;
				decode:	data ]
]

{ #category : 'printing' }
ZnDataUrl >> printOn: stream [
	stream nextPutAll: 'data:'.
	mediaType printOn: stream.
	base64Encoded
		ifTrue: [ 
			stream nextPutAll: ';base64,'.
			ZnBase64Encoder new 
				encode: data readStream to: stream ]
		ifFalse: [
			stream nextPut: $,.
			ZnPercentEncoder new 
				characterEncoder: mediaType charSet asZnCharacterEncoder;
				encode: data readStream to: stream ]
]

{ #category : 'accessing' }
ZnDataUrl >> scheme [
	"Answer my URL scheme"
	
	^ #data
]

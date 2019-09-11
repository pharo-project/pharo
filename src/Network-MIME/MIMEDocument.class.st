"
a MIME object, along with its type and the URL it was found at (if any)
"
Class {
	#name : #MIMEDocument,
	#superclass : #Object,
	#instVars : [
		'type',
		'contents',
		'contentStream',
		'uri'
	],
	#category : #'Network-MIME-Base'
}

{ #category : #'instance creation' }
MIMEDocument class >> content: aString [
	^self contentType: self defaultContentType  content: aString
]

{ #category : #'instance creation' }
MIMEDocument class >> contentStream: aStream [
	^ self 
		contentStream: aStream 
		mimeType: self defaultMIMEType
]

{ #category : #'instance creation' }
MIMEDocument class >> contentStream: aStream mimeType: aMimeType [ 
	"create a MIMEDocument with the given content-type and contentStream"
	"MIMEDocument mimeType: 'text/plain' asMIMEType contentStream: (ReadStream on: 'This is a test')"
	
	^self contentStream: aStream mimeType: aMimeType uri: aStream uri
]

{ #category : #'instance creation' }
MIMEDocument class >> contentStream: aStream mimeType: aMimeType uri: aURI [
	"create a MIMEDocument with the given content-type and contentStream"
	"MIMEDocument mimeType: 'text/plain' asMIMEType contentStream: (ReadStream on: 'This is a test')"
	
	^self new contentStream: aStream mimeType: aMimeType uri: aURI
]

{ #category : #compatibility }
MIMEDocument class >> contentType: aMIMEType content: content [
	"create a MIMEDocument with the given content-type and content"
	"MIMEDocument contentType: 'text/plain' content: 'This is a test'"
	
	^ self new
		privateContent: content;
		type: aMIMEType asZnMimeType;
		yourself
]

{ #category : #compatibility }
MIMEDocument class >> contentType: type content: content url: url [
	^ self 
		contents: content 
		mimeType: type asZnMimeType 
		uri: url
]

{ #category : #'content-types' }
MIMEDocument class >> contentTypeFormData [
	^'application/x-www-form-urlencoded'
]

{ #category : #'content-types' }
MIMEDocument class >> contentTypeHtml [
	^'text/html'
]

{ #category : #'content-types' }
MIMEDocument class >> contentTypeMultipart [
	^'multipart/form-data'
]

{ #category : #'content-types' }
MIMEDocument class >> contentTypePlainText [
	^'text/plain'
]

{ #category : #'content-types' }
MIMEDocument class >> contentTypeXml [
	^'text/xml'
]

{ #category : #'instance creation' }
MIMEDocument class >> contents: aString [
	^ self 
		contents: aString 
		mimeType: self defaultMIMEType
]

{ #category : #'instance creation' }
MIMEDocument class >> contents: content mimeType: aMimeType [ 
	"create a MIMEDocument with the given content-type and content"
	"MIMEDocument mimeType: 'text/plain' asMIMEType content: 'This is a test'"
	
	^self contents: content mimeType: aMimeType uri: nil
]

{ #category : #'instance creation' }
MIMEDocument class >> contents: content mimeType: aMimeType uri: aURL [
	"create a MIMEDocument with the given content-type and content"
	"MIMEDocument mimeType: 'text/plain' asMIMEType content: 'This is a test'"
	
	^self new contents: content mimeType: aMimeType uri: aURL
]

{ #category : #compatibility }
MIMEDocument class >> defaultContentType [
	^self defaultMIMEType asString
]

{ #category : #accessing }
MIMEDocument class >> defaultMIMEType [
	^ ZnMimeType default
]

{ #category : #compatibility }
MIMEDocument class >> guessContentTypeFromExtension: ext [
	"guesses a content type from the extension"
	^(self guessTypeFromExtension: ext) asString
]

{ #category : #compatibility }
MIMEDocument class >> guessTypeFromExtension: ext [
	"guesses a content type from the extension"
	^self guessTypeFromName: ext
]

{ #category : #compatibility }
MIMEDocument class >> guessTypeFromName: url [
	"guesses a content type from the url"
	
	| extension |
	extension := url asString copyAfterLast: $..
	^ ZnMimeType
		forFilenameExtension: extension
		ifAbsent: [ nil ]
]

{ #category : #compatibility }
MIMEDocument class >> resetMIMEdatabase [
	"no-op for catching Kom override"
]

{ #category : #compatibility }
MIMEDocument >> content [
	^self contents
]

{ #category : #accessing }
MIMEDocument >> contentStream [
	"Answer a RWBinaryOrTextStream on the contents."

	contentStream
		ifNil: [contentStream := contents
				ifNil: [self contentStreamOnURI]
				ifNotNil: [(RWBinaryOrTextStream with: self contents) reset]].
	^contentStream
]

{ #category : #private }
MIMEDocument >> contentStream: aStream mimeType: aMimeType uri: aUri [
	type := aMimeType.
	contentStream := aStream.
	uri := aUri
]

{ #category : #private }
MIMEDocument >> contentStreamOnURI [
	^self uri contentStream
]

{ #category : #compatibility }
MIMEDocument >> contentType [
	^self mimeType asString
]

{ #category : #accessing }
MIMEDocument >> contents [
	"Answer the receiver's raw data. If we have a stream to read from. Read in the data, cache it and discard the stream."

	contents ifNil: [contents := self getContentFromStream].
	^contents
]

{ #category : #private }
MIMEDocument >> contents: contentStringOrBytes mimeType: aMimeType uri: aURI [
	type := aMimeType.
	contents := contentStringOrBytes.
	uri := aURI
]

{ #category : #private }
MIMEDocument >> discardContentStream [
	contentStream ifNotNil: [contentStream close].
	contentStream := nil
]

{ #category : #accessing }
MIMEDocument >> discardContents [
	contents := nil.
	self discardContentStream
]

{ #category : #private }
MIMEDocument >> getContentFromStream [
	| streamContents |
	streamContents := self contentStream contents.
	self discardContentStream.
	^streamContents
]

{ #category : #testing }
MIMEDocument >> isGif [
	^ self mainType = 'image'
		and: [self subType = 'gif']
]

{ #category : #testing }
MIMEDocument >> isJpeg [
	^ self mainType = 'image'
		and: [ #('jpeg' 'jpg') includes: self subType ]
]

{ #category : #testing }
MIMEDocument >> isMultipart [
	^self mainType = 'multipart'
]

{ #category : #testing }
MIMEDocument >> isMultipartAlternative [
	"whether the document is in a multipart format where the parts are alternates"
	^ self contentType = 'multipart/alternative'

]

{ #category : #testing }
MIMEDocument >> isPng [
	^ self mainType = 'image'
		and: [self subType = 'png']
]

{ #category : #testing }
MIMEDocument >> isPnm [
	^ self mainType = 'image'
		and: [self subType = 'pnm']
]

{ #category : #accessing }
MIMEDocument >> mainType [
	^self mimeType main
]

{ #category : #accessing }
MIMEDocument >> mimeType [
	^type
]

{ #category : #printing }
MIMEDocument >> printOn: aStream [
	aStream nextPutAll: self class name;
		nextPutAll: ' (';
		print: self mimeType;
		nextPutAll: ', '.
	contents
		ifNotNil: [ aStream
			print: self contents size;
			nextPutAll: ' bytes)' ]
		ifNil: [ aStream nextPutAll: 'unknown size)' ].
]

{ #category : #private }
MIMEDocument >> privateContent: aString [
	contents := aString
]

{ #category : #files }
MIMEDocument >> saveToFile: pathString [
	pathString asFileReference 
		binaryWriteStreamDo: [ :out | 
			out nextPutAll: self contents ]
]

{ #category : #accessing }
MIMEDocument >> subType [
	^self mimeType sub
]

{ #category : #accessing }
MIMEDocument >> type: mimeType [
	type := mimeType
]

{ #category : #accessing }
MIMEDocument >> uri [
	"Answer the URL the receiver was downloaded from.  It may legitimately be nil."

	^uri
]

{ #category : #accessing }
MIMEDocument >> uri: aURI [
	uri := aURI
]

{ #category : #accessing }
MIMEDocument >> url [
	"Answer the URL the receiver was downloaded from.  It may legitimately be nil."

	^ uri ifNotNil:[uri asString asUrl]
]

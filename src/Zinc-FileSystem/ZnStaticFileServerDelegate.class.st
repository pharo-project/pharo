"
I am ZnStaticFileServerDelegate.
I am a simple proof of concept implementation of a web server serving static files.
I handle urls with an optional prefix as requests for files in a directory.
I serve index.html or index.htm when a directory is requested and these files exist.
I do a redirect when a path that is not does not end with a / refers to directory.
I function as a delegate for ZnServer.

ZnServer startDefaultOn: 1701.
ZnServer default delegate: ((ZnStaticFileServerDelegate new) 
									prefixFromString: 'apple/macosx'; 
									directory: '/Library/WebServer/Documents' asFileReference; 
									yourself).

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnStaticFileServerDelegate,
	#superclass : #Object,
	#instVars : [
		'prefix',
		'directory',
		'mimeTypeExpirations'
	],
	#category : #'Zinc-FileSystem'
}

{ #category : #accessing }
ZnStaticFileServerDelegate class >> defaultMimeTypeExpirations [
	"Return a default dictionary mapping ZnMimeTypes to their expiration time in seconds.
	Missing entries will have no expiration time."

	| dict |
	dict := Dictionary new.
	dict at: ZnMimeType textCss put: 30 days asSeconds.
	dict at: ZnMimeType textJavascript put: 30 days asSeconds.
	dict at: ZnMimeType textPlain put: 30 days asSeconds.
	dict at: ZnMimeType textHtml put: 30 days asSeconds.
	dict at: ZnMimeType imageGif put: 30 days asSeconds.
	dict at: ZnMimeType imageJpeg put: 30 days asSeconds.
	dict at: ZnMimeType imagePng put: 30 days asSeconds.
	dict at: ZnMimeType applicationJavascript put: 30 days asSeconds.
	^ dict
]

{ #category : #private }
ZnStaticFileServerDelegate >> actualFilenameFor: uri [
	| subElements subDir entry |
	(uri isEmpty and: [ self prefix isEmpty ]) ifTrue: [ ^ self indexFileIn: self directory ].
	(self prefix isEmpty or: [ uri isEmpty not and: [ uri pathSegments beginsWith: self prefix ] ]) ifFalse: [ ^ nil ].
	subElements := (uri pathSegments allButFirst: self prefix size) reject: [ :each | each = #/ ].
	subDir := subElements allButLast inject: self directory into: [ :parent :sub | | file |
		(file := parent / sub) exists
			ifTrue: [ file ]
			ifFalse: [ ^ nil ] ]. 
	subElements isEmpty
		ifTrue: [ entry := subDir entry ] 
		ifFalse: [ | file |
			 (file := subDir / subElements last) exists 
				ifTrue: [ entry := file entry ] 
				ifFalse: [ ^ nil ] ].
	^ entry isDirectory 
		ifTrue: [ self indexFileIn: entry reference ]
		ifFalse: [ entry reference fullName ]
]

{ #category : #accessing }
ZnStaticFileServerDelegate >> directory [
	"The directory whose files I am serving"
	
	^ directory 
]

{ #category : #accessing }
ZnStaticFileServerDelegate >> directory: fileDirectory [
	"Set the directory whose files I should be serving"

	directory := fileDirectory
]

{ #category : #private }
ZnStaticFileServerDelegate >> directoryRedirectFor: uri [
	| directoryUri |
	directoryUri := uri copy addPathSegment: #/.  
	^ ZnResponse redirect: directoryUri 
]

{ #category : #private }
ZnStaticFileServerDelegate >> expirationDateFor: aMimeType [
	| expiration |
	expiration := self mimeTypeExpirations at: aMimeType ifAbsent: [ ^ nil ].
	^ ZnUtils httpDate: (DateAndTime now asUTC + expiration seconds)
]

{ #category : #public }
ZnStaticFileServerDelegate >> handleRequest: request [
	"Server delegate entry point"

	| actualFilename |
	(#(#GET #HEAD) includes: request method)
		ifFalse: [ ^ ZnResponse methodNotAllowed: request ].
	actualFilename := self actualFilenameFor: request uri.
	^ actualFilename notNil
		ifTrue: [ 
			(self redirectNeededFor: request uri actualFilename: actualFilename)
				ifTrue: [ self directoryRedirectFor: request uri ]
				ifFalse: [ self responseForFile: actualFilename fromRequest: request ] ]
		ifFalse: [ ZnResponse notFound: request uri ]
]

{ #category : #private }
ZnStaticFileServerDelegate >> indexFileIn: fileDirectory [
	#( 'index.html' 'index.htm' ) do: [ :each | | file |
			(file := fileDirectory / each) exists
				ifTrue: [ 
					^ file fullName ] ].
	^ nil

]

{ #category : #private }
ZnStaticFileServerDelegate >> maxAgeFor: aMimeType [
	| expiration |
	expiration := self mimeTypeExpirations at: aMimeType ifAbsent: [ ^ nil ].
	^ 'max-age=', expiration asString
]

{ #category : #accessing }
ZnStaticFileServerDelegate >> mimeTypeExpirations [
	"Return a dictionary mapping ZnMimeTypes to their expiration time in seconds.
	Missing entries will have no expiration time."

	^ mimeTypeExpirations ifNil: [ mimeTypeExpirations := Dictionary new ]
]

{ #category : #accessing }
ZnStaticFileServerDelegate >> mimeTypeExpirations: aDictionary [
	"Set the mapping of ZnMimeTypes to their expiration time in seconds to aDictionary.
	Missing entries will have no expiration time."

	mimeTypeExpirations := aDictionary
]

{ #category : #accessing }
ZnStaticFileServerDelegate >> prefix [
	"The path prefix collection under which I am serving files"
	
	^ prefix ifNil: [ prefix := #() ]
]

{ #category : #accessing }
ZnStaticFileServerDelegate >> prefix: orderedCollection [
	"Set the path prefix under which I should be serving files to orderedCollection containing path elements"
	
	prefix := orderedCollection
]

{ #category : #accessing }
ZnStaticFileServerDelegate >> prefixFromString: string [
	"Set the path prefix under which I should be serving files to string,
	interpreting each /-separated token as a path element"

	self prefix: (string findTokens: '/')
]

{ #category : #private }
ZnStaticFileServerDelegate >> redirectNeededFor: uri actualFilename: actualFilename [
	uri isDirectoryPath ifTrue: [ ^ false ].  
	^ (actualFilename endsWith: uri lastPathSegment) not
]

{ #category : #private }
ZnStaticFileServerDelegate >> responseForFile: filename fromRequest: aRequest [
	| file entry size time mimeType fileStream entity response modified |
	file := filename asFileReference.
	entry := file entry.
	size := entry size.
	time := entry modificationTime.
	modified := true.
	aRequest headers
		at: 'If-Modified-Since'
		ifPresent: [ modified := time > (ZnUtils parseHttpDate: (aRequest headers at: 'If-Modified-Since')) ].
	modified
		ifTrue: [ 
			mimeType := ZnMimeType forFilenameExtension: file extension.
			fileStream := file readStream.
			fileStream binary.
			(entity := ZnStreamingEntity type: mimeType length: size) 
				stream: fileStream.
			(response := ZnResponse ok: entity) 
				headers at: 'Modification-Date' put: (ZnUtils httpDate: time).
			(self maxAgeFor: mimeType) 
				ifNotNil: [ :maxAge | response headers at: 'Cache-Control' put: maxAge ].
			(self expirationDateFor: mimeType)
				ifNotNil: [ :expirationDate | response headers at: 'Expires' put: expirationDate ] ]
		ifFalse: [ response := ZnResponse notModified ].
	^ response
]

{ #category : #public }
ZnStaticFileServerDelegate >> value: request [
	"I implement the generic #value: message as equivalent to #handleRequest:"
	
	^ self handleRequest: request
]

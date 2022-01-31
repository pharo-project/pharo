"
I'm a copy of ZnUrl because ZnrUrl query does not use an dictionary garantying ordered keys.
May be we could change ZnUrl to be able to change the dictionary used there...
I do not have the time to address this point.

"
Class {
	#name : #ZnUrlCopy,
	#superclass : #Object,
	#instVars : [
		'scheme',
		'host',
		'port',
		'segments',
		'query',
		'fragment',
		'username',
		'password'
	],
	#category : #'Microdown-Core'
}

{ #category : #accessing }
ZnUrlCopy class >> defaultPortForScheme: scheme [
	(#(http ws) includes: scheme) ifTrue: [ ^ 80 ].
	(#(https wss) includes: scheme) ifTrue: [ ^ 443 ].
	^ nil
]

{ #category : #'instance creation' }
ZnUrlCopy class >> fromString: string [
	^ (self new)
		parseFrom: string; 
		yourself 
]

{ #category : #'instance creation' }
ZnUrlCopy class >> fromString: string defaultScheme: defaultScheme [
	^ (self new)
		parseFrom: string defaultScheme: defaultScheme; 
		yourself 
]

{ #category : #accessing }
ZnUrlCopy class >> image [
	"Return a File URL refering to the file path of the current image file.
	This is useful as a context to resolve relative URLs, see #withRelativeReference:"
	
	^ SmalltalkImage current imagePath asPath asZnUrl
]

{ #category : #accessing }
ZnUrlCopy class >> schemesNotUsingDoubleSlash [
	"Most URL schemes use a double slash, as in http://
	but some don't, return a list of those"
	
	^ #( #mailto #telnet )
]

{ #category : #accessing }
ZnUrlCopy class >> schemesNotUsingPath [
	"Most URL schemes use a hierarchical path
	but some don't, return a list of those"
	
	^ #( #mailto )
]

{ #category : #convenience }
ZnUrlCopy >> & association [
	^ self withQuery: association 
]

{ #category : #convenience }
ZnUrlCopy >> + string [
	^ self withRelativeReference: string
]

{ #category : #convenience }
ZnUrlCopy >> / object [
	^ object addedToZnUrl: self
]

{ #category : #comparing }
ZnUrlCopy >> = anObject [
	^ self == anObject
		or: [ self class == anObject class and: [ self equals: anObject ] ]
]

{ #category : #convenience }
ZnUrlCopy >> ? association [
	^ self withQuery: association 
]

{ #category : #'accessing - path' }
ZnUrlCopy >> addPathSegment: segment [
	"Modify the receiver's path by adding segment at the end"
	
	segments isNil ifTrue: [ 
		segments := OrderedCollection new ].
	segments add: segment
]

{ #category : #'accessing - path' }
ZnUrlCopy >> addPathSegments: pathSegments [
	"Modify the receiver's path by adding the elements of pathSegments at the end"

	pathSegments do: [ :each | self addPathSegment: each ]
]

{ #category : #convenience }
ZnUrlCopy >> addedToZnUrl: url [
	^ url withPathSegments: self pathSegments 
]

{ #category : #converting }
ZnUrlCopy >> asFileReference [
	"Convert the receiver into a new FileReference object.
	Note that for a FileReference a trailing slash is not relevant"

	| path |
	self isSlash
		ifTrue: [ ^ FileSystem root ].
	path := self isDirectoryPath
		ifTrue: [ segments allButLast ]
		ifFalse: [ segments copy ].
	^ FileReference fileSystem: FileSystem disk path: (AbsolutePath withAll: path)
]

{ #category : #converting }
ZnUrlCopy >> asFileUrl [
	"Return a copy of the receiver as a File URL, replacing the scheme with #file.
	This is useful when you want to do a #asFileReference afterwards."
	
	^ self copy
		scheme: #file;
		yourself
]

{ #category : #converting }
ZnUrlCopy >> asRelativeUrl [
	"Copy the receiver, with scheme, host and port cleared"
	
	^ self copy
		scheme: nil;
		host: nil;
		port: nil;
		yourself
]

{ #category : #converting }
ZnUrlCopy >> asUrl [
	^ self
]

{ #category : #converting }
ZnUrlCopy >> asZnUrl [
	^ self
]

{ #category : #converting }
ZnUrlCopy >> asZnUrlWithDefaults [
	^ self copy 
		setDefaults; 
		yourself
]

{ #category : #accessing }
ZnUrlCopy >> authority [
	^ self hasNonDefaultPort 
		ifTrue: [ 
			String streamContents: [ :stream | 
				stream nextPutAll: self host; nextPut: $:; print: self port ] ]
		ifFalse: [ 
			self host ]
]

{ #category : #accessing }
ZnUrlCopy >> authorityWithPort [
	^ String streamContents: [ :stream | 
			stream nextPutAll: self host; nextPut: $:; print: self portOrDefault ] 
]

{ #category : #'accessing - path' }
ZnUrlCopy >> clearPath [
	self hasPath
		ifTrue: [ segments removeAll ]
]

{ #category : #'accessing - path' }
ZnUrlCopy >> closePath [
	self isDirectoryPath
		ifFalse: [ self addPathSegment: #/ ]
]

{ #category : #private }
ZnUrlCopy >> decodePercent: string [
	^ ZnResourceMetaUtils decodePercent: string
]

{ #category : #'accessing - path' }
ZnUrlCopy >> directory [
	^ self isDirectoryPath
		ifTrue: [ self path ]
		ifFalse: [ 
			String streamContents: [ :stream |
				segments allButLast  
					do: [ :each | stream nextPutAll: each ]
					separatedBy: [ stream nextPut: $/ ] ] ]
]

{ #category : #private }
ZnUrlCopy >> encode: string on: stream [
	stream nextPutAll: (ZnResourceMetaUtils encodePercent: string safeSet: #rfc3986SafeSet encoder: ZnCharacterEncoder utf8)
]

{ #category : #private }
ZnUrlCopy >> encodePath: string on: stream [
	stream nextPutAll: (ZnResourceMetaUtils encodePercent: string safeSet: #urlPathSafeSet encoder: ZnCharacterEncoder utf8)
]

{ #category : #private }
ZnUrlCopy >> encodeQuery: string on: stream [
	stream nextPutAll: (ZnResourceMetaUtils encodePercent: string safeSet: #querySafeSet encoder: ZnCharacterEncoder utf8)
]

{ #category : #convenience }
ZnUrlCopy >> enforceKnownScheme [
	(scheme isNil or: [ #(#http #https #ws #wss #file) includes: scheme ])
		ifFalse: [ (ZnUnknownScheme scheme: scheme) signal ]
]

{ #category : #comparing }
ZnUrlCopy >> equals: url [
	self scheme = url scheme ifFalse: [ ^ false ].
	self host = url host ifFalse: [ ^ false ].
	self portOrDefault = url portOrDefault ifFalse: [ ^ false ].
	((self isSlash and: [ url isSlash ]) or: [ self segments = url segments ]) ifFalse: [ ^ false ].
	(self hasQuery or: [ url hasQuery ])
		ifTrue: [ self query = url query ifFalse: [ ^ false ] ].
	self fragment = url fragment ifFalse: [ ^ false ].
	^ true
]

{ #category : #'accessing - path' }
ZnUrlCopy >> file [
	^ self isDirectoryPath
		ifTrue: [ String new ]
		ifFalse: [ segments last ]
]

{ #category : #'accessing - path' }
ZnUrlCopy >> firstPathSegment [
	^ self isSlash
		ifTrue: [ nil ]
		ifFalse: [ segments first ]
]

{ #category : #accessing }
ZnUrlCopy >> fragment [
	^ fragment
]

{ #category : #accessing }
ZnUrlCopy >> fragment: string [
	fragment := string
]

{ #category : #testing }
ZnUrlCopy >> hasFragment [
	^ fragment notNil
]

{ #category : #testing }
ZnUrlCopy >> hasHost [
	^ host notNil
]

{ #category : #testing }
ZnUrlCopy >> hasNonDefaultPort [
	^ self hasPort 
		and: [ self port ~= (self class defaultPortForScheme: self schemeOrDefault) ]
]

{ #category : #testing }
ZnUrlCopy >> hasPassword [
	^ password notNil
]

{ #category : #testing }
ZnUrlCopy >> hasPath [
	^ segments notNil and: [ segments isEmpty not ]
]

{ #category : #testing }
ZnUrlCopy >> hasPort [
	^ port notNil
]

{ #category : #testing }
ZnUrlCopy >> hasQuery [
	^ query notNil and: [ query isEmpty not ]
]

{ #category : #testing }
ZnUrlCopy >> hasScheme [
	^ scheme notNil
]

{ #category : #testing }
ZnUrlCopy >> hasSecureScheme [
	^ #(https wss) includes: self scheme
]

{ #category : #testing }
ZnUrlCopy >> hasUsername [
	^ username notNil
]

{ #category : #comparing }
ZnUrlCopy >> hash [
	^ self hasPath
		ifTrue: [ segments inject: host hash into: [ :sum :each | sum bitXor: each hash ] ]
		ifFalse: [ host hash ]
]

{ #category : #accessing }
ZnUrlCopy >> host [
	^ host
]

{ #category : #accessing }
ZnUrlCopy >> host: hostName [
	host := hostName ifNotNil: [ hostName asLowercase ]
]

{ #category : #converting }
ZnUrlCopy >> inContextOf: absoluteUrl [
	"Return a copy of the receiver where scheme, host and port
	are taken from absoluteUrl unless they are already in the receiver.
	Path merging is not supported. See also #withRelativeReference: "
	
	| copy |
	copy := self copy.
	copy hasScheme ifFalse: [ copy scheme: absoluteUrl scheme ].
	copy hasHost ifFalse: [ copy host: absoluteUrl host ].
	(copy hasPort not and: [ copy scheme = absoluteUrl scheme ]) 
		ifTrue: [ copy port: absoluteUrl port ].      
	^ copy
]

{ #category : #testing }
ZnUrlCopy >> isAbsolute [
	"We consider URLs with scheme://host as absolute (port is not relevant here).
	See also #inContextOf: and #withRelativeReference:"
	
	^ self hasScheme and: [ self hasHost ]
]

{ #category : #testing }
ZnUrlCopy >> isDirectoryPath [
	^ self hasPath 
		ifTrue: [ segments last = #/ ]
		ifFalse: [ true ]
	
]

{ #category : #testing }
ZnUrlCopy >> isEmpty [
	^ (self hasScheme | self hasHost | self hasPath | self hasQuery | self hasFragment | self hasUsername | self hasPassword) not
]

{ #category : #testing }
ZnUrlCopy >> isFile [
	^ scheme = #file
]

{ #category : #testing }
ZnUrlCopy >> isFilePath [
	^ self isDirectoryPath not
]

{ #category : #testing }
ZnUrlCopy >> isHttp [
	^ scheme == #http
]

{ #category : #testing }
ZnUrlCopy >> isHttps [
	^ scheme == #https
]

{ #category : #testing }
ZnUrlCopy >> isLocalHost [
	^ self hasHost and: [ #('localhost' '127.0.0.1') includes: self host ]
]

{ #category : #testing }
ZnUrlCopy >> isRelative [
	^ self isAbsolute not
]

{ #category : #private }
ZnUrlCopy >> isSchemeNotUsingDoubleSlash: schemeString [
	^ self class schemesNotUsingDoubleSlash includes: schemeString asLowercase
]

{ #category : #testing }
ZnUrlCopy >> isSchemeUsingDoubleSlash [
	^ (self class schemesNotUsingDoubleSlash includes: self scheme) not
]

{ #category : #testing }
ZnUrlCopy >> isSchemeUsingPath [
	^ (self class schemesNotUsingPath includes: self scheme) not
]

{ #category : #testing }
ZnUrlCopy >> isSlash [
	^ self hasPath not or: [ segments size = 1 and: [ segments first = #/ ] ]
]

{ #category : #testing }
ZnUrlCopy >> isWeb [

	^ #( http https ) includes: self scheme
]

{ #category : #'accessing - path' }
ZnUrlCopy >> lastPathSegment [
	^ self isSlash
		ifTrue: [ nil ]
		ifFalse: [ segments last ]
]

{ #category : #accessing }
ZnUrlCopy >> mailToAddress [
	"Assuming my scheme is #mailto, return the address."
	
	^ self username, '@', self host 
]

{ #category : #parsing }
ZnUrlCopy >> parseAuthority: string from: start to: stop [
	| index |
	((index := string indexOf: $@ startingAt: start) > 0 and: [ index < stop ])
		ifTrue: [
			self parseUserInfo: (ReadStream on: string from: start to: index - 1).
			self parseHostPort: (ReadStream on: string from: index + 1 to: stop) ]
		ifFalse: [ 
			self parseHostPort: (ReadStream on: string from: start to: stop) ]
]

{ #category : #parsing }
ZnUrlCopy >> parseFrom: string [
	self parseFrom: string defaultScheme: nil
]

{ #category : #parsing }
ZnUrlCopy >> parseFrom: string defaultScheme: defaultScheme [
	| start end index |
	start := 1.
	end := string size.
	(index := string indexOf: $#) > 0
		ifTrue: [ 
			self fragment: (self decodePercent: (string copyFrom: index + 1 to: end)).
			end := index - 1 ].
	(index := string indexOf: $?) > 0
		ifTrue: [
			self query: (ZnResourceMetaUtils parseQueryFrom: (ReadStream on: string from: index + 1 to: end)).
			end := index - 1 ].
	((index := string indexOfSubCollection: '://') > 0 and: [ index <= end ])
		ifTrue: [
			self scheme: (string copyFrom: 1 to: index - 1).
			start := index + 3 ]
		ifFalse: [ 
			((index := string indexOf: $:) > 0 
					and: [ index <= end 
						and: [ self isSchemeNotUsingDoubleSlash: (string copyFrom: 1 to: index - 1) ] ])
				ifTrue: [ 
					self scheme: (string copyFrom: 1 to: index - 1).
					start := index + 1 ]
				ifFalse: [ 
					defaultScheme ifNotNil: [ self scheme: defaultScheme ] ] ].
	self hasScheme 
		ifTrue: [  
			(index := string indexOf: $/ startingAt: start) > 0
				ifTrue: [
					self parseAuthority: string from: start to: index - 1.
					start := index ]
				ifFalse: [ 
					^ self parseAuthority: string from: start to: end ] ].
	self parsePath: (ReadStream on: string from: start to: end)
]

{ #category : #parsing }
ZnUrlCopy >> parseHostPort: stream [
	| hostString portNumber |
	self isFile
		ifTrue: [ 
			(hostString := stream upTo: $/) isEmpty
				ifFalse: [ self host: (self decodePercent: hostString) ] ]
		ifFalse: [ 
			(hostString := stream upTo: $:) isEmpty
				ifFalse: [ self host: (self decodePercent: hostString) ].
			stream atEnd
				ifFalse: [
					portNumber := Integer readFrom: stream ifFail: [ ZnPortNotANumber signal ].
					(portNumber between: 1 and: 65535) ifFalse: [ DomainError signalFrom: 1 to: 65535 ].
					self port: portNumber ] ]
]

{ #category : #parsing }
ZnUrlCopy >> parsePath: stream [
	stream peekFor: $/.
	[ stream atEnd ] whileFalse: [ | segment |
		segment := String streamContents: [ :stringStream |
			[ stream atEnd not and: [ stream peek ~= $/ ] ] whileTrue: [
				stringStream nextPut: stream next ] ].
		segment = '.'
			ifFalse: [
				segment = '..' 
					ifTrue: [ self removeLastPathSegment ]
					ifFalse: [ self addPathSegment: (self decodePercent: segment) ] ].
		((stream peekFor: $/) and: [ stream atEnd ])
			ifTrue: [ self closePath ] ]
]

{ #category : #parsing }
ZnUrlCopy >> parseUserInfo: stream [
	| userString |
	(userString := stream upTo: $:) isEmpty 
		ifFalse: [  
			self username: (self decodePercent: userString) ].
	stream atEnd 
		ifFalse: [ 
			self password: (self decodePercent: stream upToEnd) ]
]

{ #category : #accessing }
ZnUrlCopy >> password [
	^ password
]

{ #category : #accessing }
ZnUrlCopy >> password: string [
	password := string
]

{ #category : #'accessing - path' }
ZnUrlCopy >> path [
	self hasPath ifFalse: [ ^ String new ].
	^ String streamContents: [ :stream | 
		segments 
			do: [ :each | 
				each == #/
					ifFalse: [ stream nextPutAll: each ] ]
			separatedBy: [ stream nextPut: $/ ] ]
]

{ #category : #printing }
ZnUrlCopy >> pathPrintString [
	^ String streamContents: [ :stream | 
			self printPathOn: stream ]
]

{ #category : #printing }
ZnUrlCopy >> pathQueryFragmentPrintString [
	^ String streamContents: [ :stream | 
			self printPathQueryFragmentOn: stream ]
]

{ #category : #'accessing - path' }
ZnUrlCopy >> pathSegments [
	^ segments ifNil: [ #() ]
]

{ #category : #operations }
ZnUrlCopy >> performOperation: operation [
	"Look for and execute a handler that can perform operation on the receiver"
	
	^ self performOperation: operation with: nil
]

{ #category : #operations }
ZnUrlCopy >> performOperation: operation with: argument [
	"Look for and execute a handler that can perform operation 
	on the receiver with the optional argument"
	
	^ ZnUrlOperation
		performOperation: operation 
		with: argument 
		on: self 
]

{ #category : #accessing }
ZnUrlCopy >> port [
	^ port
]

{ #category : #accessing }
ZnUrlCopy >> port: integer [
	port := integer
]

{ #category : #accessing }
ZnUrlCopy >> portIfAbsent: valuable [
	^ self hasPort
		ifTrue: [ self port ]
		ifFalse: valuable
]

{ #category : #accessing }
ZnUrlCopy >> portOrDefault [
	^ self portIfAbsent: [ self class defaultPortForScheme: self schemeOrDefault ]
]

{ #category : #copying }
ZnUrlCopy >> postCopy [
	super postCopy.
	segments := segments copy.
	query := query copy
]

{ #category : #printing }
ZnUrlCopy >> printAuthorityOn: stream [
	self hasUsername ifTrue: [
		self encode: self username on: stream.
		self hasPassword ifTrue: [
			stream nextPut: $:.
			self encode: self password on: stream ].
		stream nextPut: $@ ].
	self hasHost ifTrue: [ 
		self encode: self host on: stream ].
	self hasPort ifTrue: [ 
		stream nextPut: $:; print: self port ]

]

{ #category : #printing }
ZnUrlCopy >> printOn: stream [
	self hasScheme ifTrue: [ 
		stream nextPutAll: self scheme; nextPut: $:.
		self isSchemeUsingDoubleSlash ifTrue: [ stream nextPut: $/; nextPut: $/ ] ].
	self printAuthorityOn: stream.
	self printPathQueryFragmentOn: stream
]

{ #category : #printing }
ZnUrlCopy >> printPathOn: stream [
	self hasPath ifFalse: [ ^ stream nextPut: $/ ].
 	segments do: [ :each | 
		stream nextPut: $/.
		each == #/
			ifFalse: [ 
				self encodePath: each on: stream ] ] 
]

{ #category : #printing }
ZnUrlCopy >> printPathQueryFragmentOn: stream [
	self isSchemeUsingPath 
		ifTrue: [ self printPathOn: stream ].
	self hasQuery
		ifTrue: [ self printQueryOn: stream ].
	self hasFragment
		ifFalse: [ ^ self ].
	stream nextPut: $#.
	self encodeQuery: self fragment on: stream
]

{ #category : #printing }
ZnUrlCopy >> printQueryOn: stream [
	stream nextPut: $?. 
	ZnResourceMetaUtils writeQueryFields: self query on: 	stream
]

{ #category : #private }
ZnUrlCopy >> processRelativeReference: reference [
	"Parse & resolve the relative reference using myself as base URL, destructively"
	
	| index end |
	reference first = $/ ifTrue: [ self clearPath ].
	(self isFilePath and: [ ('?#' includes: reference first) not ] ) ifTrue: [ self removeLastPathSegment ].
	end := reference size.
	(index := reference indexOf: $#) > 0
		ifTrue: [ 
			self fragment: (self decodePercent: (reference copyFrom: index + 1 to: end)).
			end := index - 1 ].
	(index := reference indexOf: $?) > 0
		ifTrue: [
			self query: (ZnResourceMetaUtils parseQueryFrom: (ReadStream on: reference from: index + 1 to: end)).
			end := index - 1 ].
	self parsePath: (ReadStream on: reference from: 1 to: end).
	(reference = '.' ) | (reference = '..' ) | (reference endsWith: '/.') | (reference endsWith: '/..') 
		ifTrue: [ self closePath ]
]

{ #category : #accessing }
ZnUrlCopy >> query [
	^ query
]

{ #category : #accessing }
ZnUrlCopy >> query: dictionary [
	query := dictionary
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryAddAll: keyedCollection [
	"Add all key/value pairs in keyedCollection as query parameters to the receiver.
	Note that we use #addAllMulti:"

	keyedCollection isEmpty
		ifFalse: [ 
			query ifNil: [ query := OrderedDictionary new ].
			query addAllMulti: keyedCollection ].
	^ keyedCollection
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryAt: key [
	"Return the value of the query parameter key in the receiver.
	Signal a KeyNotFound exception if there is no such parameter"
	
	^ self queryAt: key ifAbsent: [ KeyNotFound signalFor: key printString ]
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryAt: key add: value [
	"Modify the receiver by adding a query variable key=value.
	If key already exists, value is added to it."

	query isNil 
		ifTrue: [ query := ZnMultiValueDictionary new ].
	query at: key asString add: (value ifNotNil: [value asString])
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryAt: key ifAbsent: block [
	"Return the value of the query parameter key in the receiver.
	Execute block if there is no such parameter"
	
	^ self hasQuery
		ifTrue: [ self query at: key asString ifAbsent: block ]
		ifFalse: block
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryAt: key ifPresent: block [
	"Execute block with the value of the query parameter named key as value.
	Return nil if there is no such parameter"
	
	^ self hasQuery
		ifTrue: [ self query at: key asString ifPresent: block ]
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryAt: key put: value [
	"Modify the receiver by setting a query variable key=value.
	If key is already exists, it is overwritten."

	query isNil 
		ifTrue: [ query := ZnMultiValueDictionary new ].
	query at: key asString put: (value ifNotNil: [value asString])
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryDo: block [
	"Execute block for each query key/value pair in the receiver"
	
	self hasQuery ifTrue: [ 
		self query keysAndValuesDo: block ]
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryKeys [
	"Return the collection of all query keys in the receiver"
	
	^ self hasQuery
		ifTrue: [ self query keys ]
		ifFalse: [ #() ]
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryRemoveAll [
	"Modify the receiver by removing all query parameters"
	
	self hasQuery
		ifTrue: [ self query removeAll ]
]

{ #category : #'accessing - query' }
ZnUrlCopy >> queryRemoveKey: key [
	"Modify the receiver by remove the query parameter named key.
	Do nothing if there is no such parameter"
	
	self hasQuery
		ifTrue: [ self query removeKey: key asString ifAbsent: [ ] ]
]

{ #category : #'accessing - path' }
ZnUrlCopy >> removeFirstPathSegment [
	self hasPath
		ifTrue: [ segments removeFirst ]
]

{ #category : #'accessing - path' }
ZnUrlCopy >> removeLastPathSegment [
	self hasPath
		ifTrue: [ segments removeLast ]
]

{ #category : #accessing }
ZnUrlCopy >> scheme [
	^ scheme
]

{ #category : #accessing }
ZnUrlCopy >> scheme: anObject [
	anObject isNil
		ifTrue: [ scheme := nil ]
		ifFalse: [ scheme := anObject asLowercase asSymbol ]
]

{ #category : #accessing }
ZnUrlCopy >> schemeOrDefault [
	^ self hasScheme 
		ifTrue: [ self scheme ]
		ifFalse: [ #http ]    
]

{ #category : #private }
ZnUrlCopy >> segments [
	^ segments
]

{ #category : #private }
ZnUrlCopy >> segments: collection [
	segments := collection
]

{ #category : #private }
ZnUrlCopy >> setDefaults [
	self hasScheme 
		ifFalse: [ self scheme: self schemeOrDefault ].
	self hasPort 
		ifFalse: [ self port: self portOrDefault ]    
]

{ #category : #accessing }
ZnUrlCopy >> username [
	^ username
]

{ #category : #accessing }
ZnUrlCopy >> username: string [
	username := string
]

{ #category : #convenience }
ZnUrlCopy >> withPathSegment: segment [
	"Return a new URL equal to the receiver with its path extended with segment"
	
	^ self copy
		addPathSegment: segment;
		yourself
]

{ #category : #convenience }
ZnUrlCopy >> withPathSegments: pathSegments [
	"Return a new URL equal to the receiver with its path extended with pathSegments"
	
	^ self copy
		addPathSegments: pathSegments;
		yourself
]

{ #category : #convenience }
ZnUrlCopy >> withQuery: association [
	"Return a new URL equal to the receiver with the association's 
	key=value added as a query parameter.
	Note that #queryAt:add: is used."
	
	^ self copy
		queryAt: association key add: association value;
		yourself 
		
]

{ #category : #parsing }
ZnUrlCopy >> withRelativeReference: reference [
	"Return a new URL by parsing & resolving the relative reference using myself as base URL.
	This implements the process described in RFC 3986 Section 5"
	
	| resolved |
	[ (resolved := reference asZnUrl) isAbsolute 
		ifTrue: [ ^ resolved ] ] on: Error do: [ ].
	(reference beginsWith: '//')
		ifTrue: [ ^ self class fromString: (reference allButFirst: 2) defaultScheme: self scheme ].
	resolved := self copy.
	resolved fragment: nil.
	reference isEmpty ifTrue: [ ^ resolved ].
	reference first = $# ifFalse: [ resolved queryRemoveAll ].
	resolved processRelativeReference: reference.
	^ resolved
]

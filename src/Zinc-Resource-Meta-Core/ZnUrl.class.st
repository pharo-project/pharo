"
I am ZnUrl, an implementation of an interpreted URL/URI.
URLs are an element used in describing resources, more specifically to identify them.

I consist of the following parts:
  - scheme - like #http, #https, #ws, #wws, #file or nil
  - host - hostname string or nil
  - port - port integer or nil
  - segments - collection of path segments, ends with #/ for directories
  - query - query dictionary or nil
  - fragment - fragment string or nil
  - username - username string or nil
  - password - password string or nil

The syntax of my external representation informally looks like this

  scheme://username:password@host:port/segments?query#fragment

I am most often created by parsing my external representation using either my #fromString: class method or by sending the #asZnUrl convenience method to a String. Using #asZnUrl helps in accepting both Strings and ZnUrls arguments.

  ZnUrl fromString: 'http://www.google.com/search?q=Smalltalk'.

I can also be constucted programmatically.

  ZnUrl new 
    scheme: #https; 
    host: 'encrypted.google.com'; 
    addPathSegment: 'search'; 
    queryAt: 'q' put: 'Smalltalk'; 
    yourself.
  
My components can be manipulated destructively. Here is an example:

  'http://www.google.com/?one=1&two=2' asZnUrl
    queryAt: 'three' put: '3';
    queryRemoveKey: 'one';
    yourself.

Some characters of parts of a URL are illegal because they would interfere with the syntax and further processing and thus have to be encoded. The methods in accessing protocols do not do any encoding, those in parsing and printing do. Here is an example:

  'http://www.google.com' asZnUrl
    addPathSegment: 'some encoding here';
    queryAt: 'and some encoding' put: 'here, too';
    yourself

My parser is somewhat forgiving and accepts some unencoded URLs as well, like most browsers would.

  'http://www.example.com:8888/a path?q=a, b, c' asZnUrl.

I can parse in the context of a default scheme, like a browser would do.

  ZnUrl fromString: 'www.example.com' defaultScheme: #http

Given a scheme, I know its default port, try #portOrDefault.

A path defaults to what is commonly referred to as slash, test with #isSlash. Paths are most often (but don't have to be) interpreted as filesystem paths. To support this, I have #isFilePath and #isDirectoryPath tests and #file and #directory accessors.

I have some support to handle one URL in the context of another one, this is also known as a relative URL in the context of an absolute URL. Refer to #isAbsolute, #isRelative and #inContextOf:

  '/folder/file.txt' asZnUrl inContextOf: 'http://fileserver.example.net:4400' asZnUrl.

Incomplete relative references can be parsed and resolved in the context of a base URL using #withRelativeReference:

  'http://www.site.com/static/html/home.html' asZnUrl withRelativeReference: '../js/menu.js'.

Sometimes, the combination of my host and port are referred to as authority, see #authority.

URL/URI/URN (Uniform/Universal Resource Locator/Identifier/Name) are closely related and can be and are used as synonyms is many contexts. Refer to http://en.wikipedia.org/wiki/Url for more information.

There is a convenience method #retrieveContents to download the resource a ZnUrl points to,

  'http://zn.stfx.eu/zn/numbers.txt' asZnUrl retrieveContents.

This is implemented using a ZnUrlOperation.
 
Part of Zinc HTTP Components.
"
Class {
	#name : #ZnUrl,
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
	#category : #Zinc-Resource-Meta-Core
}

{ #category : #accessing }
ZnUrl class >> defaultPortForScheme: scheme [
	(#(http ws) includes: scheme) ifTrue: [ ^ 80 ].
	(#(https wss) includes: scheme) ifTrue: [ ^ 443 ].
	^ nil
]

{ #category : #'instance creation' }
ZnUrl class >> fromString: string [
	^ (self new)
		parseFrom: string; 
		yourself 
]

{ #category : #'instance creation' }
ZnUrl class >> fromString: string defaultScheme: defaultScheme [
	^ (self new)
		parseFrom: string defaultScheme: defaultScheme; 
		yourself 
]

{ #category : #accessing }
ZnUrl class >> image [
	"Return a File URL refering to the file path of the current image file.
	This is useful as a context to resolve relative URLs, see #withRelativeReference:"
	
	^ SmalltalkImage current imagePath asPath asZnUrl
]

{ #category : #accessing }
ZnUrl class >> schemesNotUsingDoubleSlash [
	"Most URL schemes use a double slash, as in http://
	but some don't, return a list of those"
	
	^ #( #mailto #telnet )
]

{ #category : #accessing }
ZnUrl class >> schemesNotUsingPath [
	"Most URL schemes use a hierarchical path
	but some don't, return a list of those"
	
	^ #( #mailto )
]

{ #category : #convenience }
ZnUrl >> & association [
	^ self withQuery: association 
]

{ #category : #convenience }
ZnUrl >> + string [
	^ self withRelativeReference: string
]

{ #category : #convenience }
ZnUrl >> / object [
	^ object addedToZnUrl: self
]

{ #category : #comparing }
ZnUrl >> = anObject [
	^ self == anObject
		or: [ self class == anObject class and: [ self equals: anObject ] ]
]

{ #category : #convenience }
ZnUrl >> ? association [
	^ self withQuery: association 
]

{ #category : #accessing-path }
ZnUrl >> addPathSegment: segment [
	"Modify the receiver's path by adding segment at the end"
	
	segments isNil ifTrue: [ 
		segments := OrderedCollection new ].
	segments add: segment
]

{ #category : #accessing-path }
ZnUrl >> addPathSegments: pathSegments [
	"Modify the receiver's path by adding the elements of pathSegments at the end"

	pathSegments do: [ :each | self addPathSegment: each ]
]

{ #category : #convenience }
ZnUrl >> addedToZnUrl: url [
	^ url withPathSegments: self pathSegments 
]

{ #category : #converting }
ZnUrl >> asFileUrl [
	"Return a copy of the receiver as a File URL, replacing the scheme with #file.
	This is useful when you want to do a #asFileReference afterwards."
	
	^ self copy
		scheme: #file;
		yourself
]

{ #category : #converting }
ZnUrl >> asRelativeUrl [
	"Copy the receiver, with scheme, host and port cleared"
	
	^ self copy
		scheme: nil;
		host: nil;
		port: nil;
		yourself
]

{ #category : #converting }
ZnUrl >> asUrl [
	^ self
]

{ #category : #converting }
ZnUrl >> asZnUrl [
	^ self
]

{ #category : #converting }
ZnUrl >> asZnUrlWithDefaults [
	^ self copy 
		setDefaults; 
		yourself
]

{ #category : #accessing }
ZnUrl >> authority [
	^ self hasNonDefaultPort 
		ifTrue: [ 
			String streamContents: [ :stream | 
				stream nextPutAll: self host; nextPut: $:; print: self port ] ]
		ifFalse: [ 
			self host ]
]

{ #category : #accessing }
ZnUrl >> authorityWithPort [
	^ String streamContents: [ :stream | 
			stream nextPutAll: self host; nextPut: $:; print: self portOrDefault ] 
]

{ #category : #accessing-path }
ZnUrl >> clearPath [
	self hasPath
		ifTrue: [ segments removeAll ]
]

{ #category : #accessing-path }
ZnUrl >> closePath [
	self isDirectoryPath
		ifFalse: [ self addPathSegment: #/ ]
]

{ #category : #private }
ZnUrl >> decodePercent: string [
	^ ZnResourceMetaUtils decodePercent: string
]

{ #category : #accessing-path }
ZnUrl >> directory [
	^ self isDirectoryPath
		ifTrue: [ self path ]
		ifFalse: [ 
			String streamContents: [ :stream |
				segments allButLast  
					do: [ :each | stream nextPutAll: each ]
					separatedBy: [ stream nextPut: $/ ] ] ]
]

{ #category : #private }
ZnUrl >> encode: string on: stream [
	stream nextPutAll: (ZnResourceMetaUtils encodePercent: string safeSet: #rfc3986SafeSet encoder: ZnCharacterEncoder utf8)
]

{ #category : #private }
ZnUrl >> encodePath: string on: stream [
	stream nextPutAll: (ZnResourceMetaUtils encodePercent: string safeSet: #urlPathSafeSet encoder: ZnCharacterEncoder utf8)
]

{ #category : #private }
ZnUrl >> encodeQuery: string on: stream [
	stream nextPutAll: (ZnResourceMetaUtils encodePercent: string safeSet: #querySafeSet encoder: ZnCharacterEncoder utf8)
]

{ #category : #convenience }
ZnUrl >> enforceKnownScheme [
	(scheme isNil or: [ #(#http #https #ws #wss #file) includes: scheme ])
		ifFalse: [ (ZnUnknownScheme scheme: scheme) signal ]
]

{ #category : #comparing }
ZnUrl >> equals: url [
	self scheme = url scheme ifFalse: [ ^ false ].
	self host = url host ifFalse: [ ^ false ].
	self portOrDefault = url portOrDefault ifFalse: [ ^ false ].
	((self isSlash and: [ url isSlash ]) or: [ self segments = url segments ]) ifFalse: [ ^ false ].
	(self hasQuery or: [ url hasQuery ])
		ifTrue: [ self query = url query ifFalse: [ ^ false ] ].
	self fragment = url fragment ifFalse: [ ^ false ].
	^ true
]

{ #category : #accessing-path }
ZnUrl >> file [
	^ self isDirectoryPath
		ifTrue: [ String new ]
		ifFalse: [ segments last ]
]

{ #category : #accessing-path }
ZnUrl >> firstPathSegment [
	^ self isSlash
		ifTrue: [ nil ]
		ifFalse: [ segments first ]
]

{ #category : #accessing }
ZnUrl >> fragment [
	^ fragment
]

{ #category : #accessing }
ZnUrl >> fragment: string [
	fragment := string
]

{ #category : #testing }
ZnUrl >> hasFragment [
	^ fragment notNil
]

{ #category : #testing }
ZnUrl >> hasHost [
	^ host notNil
]

{ #category : #testing }
ZnUrl >> hasNonDefaultPort [
	^ self hasPort 
		and: [ self port ~= (self class defaultPortForScheme: self schemeOrDefault) ]
]

{ #category : #testing }
ZnUrl >> hasPassword [
	^ password notNil
]

{ #category : #testing }
ZnUrl >> hasPath [
	^ segments notNil and: [ segments isEmpty not ]
]

{ #category : #testing }
ZnUrl >> hasPort [
	^ port notNil
]

{ #category : #testing }
ZnUrl >> hasQuery [
	^ query notNil and: [ query isEmpty not ]
]

{ #category : #testing }
ZnUrl >> hasScheme [
	^ scheme notNil
]

{ #category : #testing }
ZnUrl >> hasSecureScheme [
	^ #(https wss) includes: self scheme
]

{ #category : #testing }
ZnUrl >> hasUsername [
	^ username notNil
]

{ #category : #comparing }
ZnUrl >> hash [
	^ self hasPath
		ifTrue: [ segments inject: host hash into: [ :sum :each | sum bitXor: each hash ] ]
		ifFalse: [ host hash ]
]

{ #category : #accessing }
ZnUrl >> host [
	^ host
]

{ #category : #accessing }
ZnUrl >> host: hostName [
	host := hostName ifNil: [ nil ] ifNotNil: [ hostName asLowercase ]
]

{ #category : #converting }
ZnUrl >> inContextOf: absoluteUrl [
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
ZnUrl >> isAbsolute [
	"We consider URLs with scheme://host as absolute (port is not relevant here).
	See also #inContextOf: and #withRelativeReference:"
	
	^ self hasScheme and: [ self hasHost ]
]

{ #category : #testing }
ZnUrl >> isDirectoryPath [
	^ self hasPath 
		ifTrue: [ segments last = #/ ]
		ifFalse: [ true ]
	
]

{ #category : #testing }
ZnUrl >> isEmpty [
	^ (self hasScheme | self hasHost | self hasPath | self hasQuery | self hasFragment | self hasUsername | self hasPassword) not
]

{ #category : #testing }
ZnUrl >> isFile [
	^ scheme = #file
]

{ #category : #testing }
ZnUrl >> isFilePath [
	^ self isDirectoryPath not
]

{ #category : #testing }
ZnUrl >> isHttp [
	^ scheme == #http
]

{ #category : #testing }
ZnUrl >> isHttps [
	^ scheme == #https
]

{ #category : #testing }
ZnUrl >> isLocalHost [
	^ self hasHost and: [ #('localhost' '127.0.0.1') includes: self host ]
]

{ #category : #testing }
ZnUrl >> isRelative [
	^ self isAbsolute not
]

{ #category : #private }
ZnUrl >> isSchemeNotUsingDoubleSlash: schemeString [
	^ self class schemesNotUsingDoubleSlash includes: schemeString asLowercase
]

{ #category : #testing }
ZnUrl >> isSchemeUsingDoubleSlash [
	^ (self class schemesNotUsingDoubleSlash includes: self scheme) not
]

{ #category : #testing }
ZnUrl >> isSchemeUsingPath [
	^ (self class schemesNotUsingPath includes: self scheme) not
]

{ #category : #testing }
ZnUrl >> isSlash [
	^ self hasPath not or: [ segments size = 1 and: [ segments first = #/ ] ]
]

{ #category : #accessing-path }
ZnUrl >> lastPathSegment [
	^ self isSlash
		ifTrue: [ nil ]
		ifFalse: [ segments last ]
]

{ #category : #accessing }
ZnUrl >> mailToAddress [
	"Assuming my scheme is #mailto, return the address."
	
	^ self username, '@', self host 
]

{ #category : #parsing }
ZnUrl >> parseAuthority: string from: start to: stop [
	| index |
	((index := string indexOf: $@ startingAt: start) > 0 and: [ index < stop ])
		ifTrue: [
			self parseUserInfo: (ReadStream on: string from: start to: index - 1).
			self parseHostPort: (ReadStream on: string from: index + 1 to: stop) ]
		ifFalse: [ 
			self parseHostPort: (ReadStream on: string from: start to: stop) ]
]

{ #category : #parsing }
ZnUrl >> parseFrom: string [
	self parseFrom: string defaultScheme: nil
]

{ #category : #parsing }
ZnUrl >> parseFrom: string defaultScheme: defaultScheme [
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
ZnUrl >> parseHostPort: stream [
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
ZnUrl >> parsePath: stream [
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
ZnUrl >> parseUserInfo: stream [
	| userString |
	(userString := stream upTo: $:) isEmpty 
		ifFalse: [  
			self username: (self decodePercent: userString) ].
	stream atEnd 
		ifFalse: [ 
			self password: (self decodePercent: stream upToEnd) ]
]

{ #category : #accessing }
ZnUrl >> password [
	^ password
]

{ #category : #accessing }
ZnUrl >> password: string [
	password := string
]

{ #category : #accessing-path }
ZnUrl >> path [
	self hasPath ifFalse: [ ^ String new ].
	^ String streamContents: [ :stream | 
		segments 
			do: [ :each | 
				each == #/
					ifFalse: [ stream nextPutAll: each ] ]
			separatedBy: [ stream nextPut: $/ ] ]
]

{ #category : #printing }
ZnUrl >> pathPrintString [
	^ String streamContents: [ :stream | 
			self printPathOn: stream ]
]

{ #category : #printing }
ZnUrl >> pathQueryFragmentPrintString [
	^ String streamContents: [ :stream | 
			self printPathQueryFragmentOn: stream ]
]

{ #category : #accessing-path }
ZnUrl >> pathSegments [
	^ segments ifNil: [ #() ]
]

{ #category : #operations }
ZnUrl >> performOperation: operation [
	"Look for and execute a handler that can perform operation on the receiver"
	
	^ self performOperation: operation with: nil
]

{ #category : #operations }
ZnUrl >> performOperation: operation with: argument [
	"Look for and execute a handler that can perform operation 
	on the receiver with the optional argument"
	
	^ ZnUrlOperation
		performOperation: operation 
		with: argument 
		on: self 
]

{ #category : #accessing }
ZnUrl >> port [
	^ port
]

{ #category : #accessing }
ZnUrl >> port: integer [
	port := integer
]

{ #category : #accessing }
ZnUrl >> portIfAbsent: valuable [
	^ self hasPort
		ifTrue: [ self port ]
		ifFalse: valuable
]

{ #category : #accessing }
ZnUrl >> portOrDefault [
	^ self portIfAbsent: [ self class defaultPortForScheme: self schemeOrDefault ]
]

{ #category : #copying }
ZnUrl >> postCopy [
	super postCopy.
	segments := segments copy.
	query := query copy
]

{ #category : #printing }
ZnUrl >> printAuthorityOn: stream [
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
ZnUrl >> printOn: stream [
	self hasScheme ifTrue: [ 
		stream nextPutAll: self scheme; nextPut: $:.
		self isSchemeUsingDoubleSlash ifTrue: [ stream nextPut: $/; nextPut: $/ ] ].
	self printAuthorityOn: stream.
	self printPathQueryFragmentOn: stream
]

{ #category : #printing }
ZnUrl >> printPathOn: stream [
	self hasPath ifFalse: [ ^ stream nextPut: $/ ].
 	segments do: [ :each | 
		stream nextPut: $/.
		each == #/
			ifFalse: [ 
				self encodePath: each on: stream ] ] 
]

{ #category : #printing }
ZnUrl >> printPathQueryFragmentOn: stream [
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
ZnUrl >> printQueryOn: stream [
	stream nextPut: $?. 
	ZnResourceMetaUtils writeQueryFields: self query on: 	stream
]

{ #category : #private }
ZnUrl >> processRelativeReference: reference [
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
ZnUrl >> query [
	^ query
]

{ #category : #accessing }
ZnUrl >> query: dictionary [
	query := dictionary
]

{ #category : #accessing-query }
ZnUrl >> queryAddAll: keyedCollection [
	"Add all key/value pairs in keyedCollection as query parameters to the receiver.
	Note that we use #addAllMulti:"

	keyedCollection isEmpty
		ifFalse: [ 
			query ifNil: [ query := ZnMultiValueDictionary new ].
			query addAllMulti: keyedCollection ].
	^ keyedCollection
]

{ #category : #accessing-query }
ZnUrl >> queryAt: key [
	"Return the value of the query parameter key in the receiver.
	Signal a KeyNotFound exception if there is no such parameter"
	
	^ self queryAt: key ifAbsent: [ KeyNotFound signalFor: key printString ]
]

{ #category : #accessing-query }
ZnUrl >> queryAt: key add: value [
	"Modify the receiver by adding a query variable key=value.
	If key already exists, value is added to it."

	query isNil 
		ifTrue: [ query := ZnMultiValueDictionary new ].
	query at: key asString add: (value ifNotNil: [value asString])
]

{ #category : #accessing-query }
ZnUrl >> queryAt: key ifAbsent: block [
	"Return the value of the query parameter key in the receiver.
	Execute block if there is no such parameter"
	
	^ self hasQuery
		ifTrue: [ self query at: key asString ifAbsent: block ]
		ifFalse: block
]

{ #category : #accessing-query }
ZnUrl >> queryAt: key ifPresent: block [
	"Execute block with the value of the query parameter named key as value.
	Return nil if there is no such parameter"
	
	^ self hasQuery
		ifTrue: [ self query at: key asString ifPresent: block ]
]

{ #category : #accessing-query }
ZnUrl >> queryAt: key put: value [
	"Modify the receiver by setting a query variable key=value.
	If key is already exists, it is overwritten."

	query isNil 
		ifTrue: [ query := ZnMultiValueDictionary new ].
	query at: key asString put: (value ifNotNil: [value asString])
]

{ #category : #accessing-query }
ZnUrl >> queryDo: block [
	"Execute block for each query key/value pair in the receiver"
	
	self hasQuery ifTrue: [ 
		self query keysAndValuesDo: block ]
]

{ #category : #accessing-query }
ZnUrl >> queryKeys [
	"Return the collection of all query keys in the receiver"
	
	^ self hasQuery
		ifTrue: [ self query keys ]
		ifFalse: [ #() ]
]

{ #category : #accessing-query }
ZnUrl >> queryRemoveAll [
	"Modify the receiver by removing all query parameters"
	
	self hasQuery
		ifTrue: [ self query removeAll ]
]

{ #category : #accessing-query }
ZnUrl >> queryRemoveKey: key [
	"Modify the receiver by remove the query parameter named key.
	Do nothing if there is no such parameter"
	
	self hasQuery
		ifTrue: [ self query removeKey: key asString ifAbsent: [ ] ]
]

{ #category : #accessing-path }
ZnUrl >> removeFirstPathSegment [
	self hasPath
		ifTrue: [ segments removeFirst ]
]

{ #category : #accessing-path }
ZnUrl >> removeLastPathSegment [
	self hasPath
		ifTrue: [ segments removeLast ]
]

{ #category : #accessing }
ZnUrl >> scheme [
	^ scheme
]

{ #category : #accessing }
ZnUrl >> scheme: anObject [
	anObject isNil
		ifTrue: [ scheme := nil ]
		ifFalse: [ scheme := anObject asLowercase asSymbol ]
]

{ #category : #accessing }
ZnUrl >> schemeOrDefault [
	^ self hasScheme 
		ifTrue: [ self scheme ]
		ifFalse: [ #http ]    
]

{ #category : #private }
ZnUrl >> segments [
	^ segments
]

{ #category : #private }
ZnUrl >> segments: collection [
	segments := collection
]

{ #category : #private }
ZnUrl >> setDefaults [
	self hasScheme 
		ifFalse: [ self scheme: self schemeOrDefault ].
	self hasPort 
		ifFalse: [ self port: self portOrDefault ]    
]

{ #category : #accessing }
ZnUrl >> username [
	^ username
]

{ #category : #accessing }
ZnUrl >> username: string [
	username := string
]

{ #category : #convenience }
ZnUrl >> withPathSegment: segment [
	"Return a new URL equal to the receiver with its path extended with segment"
	
	^ self copy
		addPathSegment: segment;
		yourself
]

{ #category : #convenience }
ZnUrl >> withPathSegments: pathSegments [
	"Return a new URL equal to the receiver with its path extended with pathSegments"
	
	^ self copy
		addPathSegments: pathSegments;
		yourself
]

{ #category : #convenience }
ZnUrl >> withQuery: association [
	"Return a new URL equal to the receiver with the association's 
	key=value added as a query parameter.
	Note that #queryAt:add: is used."
	
	^ self copy
		queryAt: association key add: association value;
		yourself 
		
]

{ #category : #parsing }
ZnUrl >> withRelativeReference: reference [
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

"
I represent an HTTP Cookie, a small piece of data sent by an HTTP web server to an HTTP user-agent client and by the client to the same server afterwards, to remember stateful information across indepedent request/responses.

References

https://en.wikipedia.org/wiki/HTTP_cookie
https://tools.ietf.org/html/rfc6265

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnCookie,
	#superclass : #Object,
	#instVars : [
		'attributes'
	],
	#category : #'Zinc-HTTP-Support'
}

{ #category : #'instance creation' }
ZnCookie class >> fromString: aString [
	^ self new 
		fromString: aString; 
		yourself
]

{ #category : #'instance creation' }
ZnCookie class >> fromString: aString for: urlObject [
	| cookie |
	cookie := self fromString: aString.
	"Ensures a domain value if none provided by the cookie"
	cookie defaultDomain: urlObject host.
	^ cookie
]

{ #category : #'instance creation' }
ZnCookie class >> name: nameString value: value [
	^ self new
		name: nameString;
		value: value;
		yourself
]

{ #category : #comparing }
ZnCookie >> = aCookie [
	(aCookie isKindOf: self class) ifFalse: [ ^ false ].
	^ (aCookie name = self name) & (aCookie path = self path) & (aCookie domain = self domain)
	
]

{ #category : #'initialize-release' }
ZnCookie >> defaultDomain: aDomain [
	self domain ifNil: [ self domain: aDomain ]
]

{ #category : #accessing }
ZnCookie >> domain [
	^ attributes at: 'domain' ifAbsent: [ nil ]
]

{ #category : #accessing }
ZnCookie >> domain: aDomain [
	attributes at: 'domain' put: aDomain
]

{ #category : #accessing }
ZnCookie >> expires [
	^ attributes at: 'expires' ifAbsent: [ nil ]
]

{ #category : #accessing }
ZnCookie >> expires: anExpiration [
	attributes at: 'expires' put: anExpiration
]

{ #category : #accessing }
ZnCookie >> expiresTimeStamp [
	"Return the DateAndTime when I will expire.
	Throw an exception when no expiration is set or the expiration cannot be parsed.
	See #isExpired"
	
	^ ZnUtils parseHttpDate: self expires
]

{ #category : #accessing }
ZnCookie >> expiresTimeStamp: dateAndTime [
	self expires: (ZnUtils httpDate: dateAndTime)
]

{ #category : #private }
ZnCookie >> extraAttributes [
	"Answer all attributes except name and value"
	^ attributes associationsSelect: [ :ea | (ea key = 'name' or: [ ea key = 'value' ]) not ]
]

{ #category : #'initialize-release' }
ZnCookie >> fromString: aString [
	| tokens val data i |
	tokens := aString substrings: ';'.
	i := tokens first indexOf: $=.
	val := tokens first copyFrom: 1 to: i - 1.
	data := tokens first copyFrom: i + 1 to: tokens first size. 
	self name: val trimBoth; value: data trimBoth.
	2 to: tokens size do: [ :index | 
		self readAttribute: (tokens at: index) ]
]

{ #category : #converting }
ZnCookie >> fullString [
	^ String streamContents: [ :stream | 
		self writeOn: stream ]
]

{ #category : #testing }
ZnCookie >> hasAttribute: attributeName [
	^ attributes includesKey: attributeName asLowercase 
]

{ #category : #comparing }
ZnCookie >> hash [
	^ (self name hash bitXor: self path hash) bitXor: self domain hash
]

{ #category : #accessing }
ZnCookie >> httponly [
	"This is not an RFC cookie attribute, but a supplement found in MS Internet Explorer and supported in PHP.
	 See http://msdn.microsoft.com/en-us/library/ms533046.aspx"
	^ attributes at: 'httponly' ifAbsent: [ false ]
]

{ #category : #accessing }
ZnCookie >> httponly: aBoolean [
	attributes at: 'httponly' put: aBoolean
]

{ #category : #initialization }
ZnCookie >> initialize [ 
	attributes := Dictionary new.
]

{ #category : #testing }
ZnCookie >> isExpired [
	| expirationTimeStamp |
	(self hasAttribute: 'expire') ifFalse: [ ^ false ].
	[ expirationTimeStamp := self expiresTimeStamp ] on: Error do: [ ^ false ].
	"note that max-age (#maxage) is not used"
	^ expirationTimeStamp asUTC < DateAndTime now asUTC
]

{ #category : #testing }
ZnCookie >> isInDomain: urlObject [
	| domain |
	domain := (self domain beginsWith: '.')
		ifTrue: [ self domain copyFrom: 2 to: self domain size ]
		ifFalse: [ self domain ].
	^ urlObject host includesSubstring: domain
	
]

{ #category : #testing }
ZnCookie >> isInPath: urlObject [
	^ urlObject pathPrintString includesSubstring: self path
	
]

{ #category : #accessing }
ZnCookie >> maxage [
	^ attributes at: 'max-age' ifAbsent: [ nil ]
]

{ #category : #accessing }
ZnCookie >> maxage: numberOfSeconds [
	"To be able to use this to validate expiration, a time of creation has to be recorded, so right now, this is not really usable."
	
	^ attributes at: 'max-age' put: numberOfSeconds asString
]

{ #category : #accessing }
ZnCookie >> name [
	^ attributes at: 'name'
]

{ #category : #accessing }
ZnCookie >> name: aName [
	attributes at: 'name' put: aName
]

{ #category : #converting }
ZnCookie >> nameValueString [
	^ String streamContents: [ :stream | 
		self writeNameAndValueOn: stream ]
]

{ #category : #accessing }
ZnCookie >> path [
	^ attributes at: 'path' ifAbsent: [ '/' ]
]

{ #category : #accessing }
ZnCookie >> path: aPath [
	attributes at: 'path' put: aPath
]

{ #category : #printing }
ZnCookie >> printOn: stream [
	self writeOn: stream
]

{ #category : #'initialize-release' }
ZnCookie >> readAttribute: aToken [
	| i key data |
	i := aToken indexOf: $=.
	i > 0
		ifTrue: [
			key := (aToken copyFrom: 1 to: i - 1) trimBoth asLowercase.
			data := (aToken copyFrom: i + 1 to: aToken size) trimBoth ]
		ifFalse: [ 
			key := (aToken copyFrom: 1 to: aToken size) trimBoth asLowercase.
			data := true ].
	attributes at: key put: data
]

{ #category : #accessing }
ZnCookie >> secure [
	^ attributes at: 'secure' ifAbsent: [ false ]
]

{ #category : #accessing }
ZnCookie >> secure: aBoolean [
	attributes at: 'secure' put: aBoolean
]

{ #category : #accessing }
ZnCookie >> value [
	^ attributes at: 'value' ifAbsent: [ nil ]
]

{ #category : #accessing }
ZnCookie >> value: aValue [
	attributes at: 'value' put: aValue
]

{ #category : #printing }
ZnCookie >> writeExtraAttributesOn: aStream [
	self extraAttributes
		associationsDo: [ :ea | 
			aStream
				nextPutAll: '; ';
				nextPutAll: ea key.
			(ea value isKindOf: Boolean)
				ifFalse: [ 
					aStream
						nextPutAll: '=';
						nextPutAll: ea value ] ]
]

{ #category : #printing }
ZnCookie >> writeNameAndValueOn: aStream [
	aStream
		nextPutAll: self name;
		nextPutAll: '=';
		nextPutAll: self value
]

{ #category : #printing }
ZnCookie >> writeOn: aStream [
	self
		writeNameAndValueOn: aStream;
		writeExtraAttributesOn: aStream 
]

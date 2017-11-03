"
I am ZnDigestCredential, a concrete subclass of ZnCredential and subclass of ZnBasicCredential.
I implement digest HTTP authentication.
I hold a nounce and opaque.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnDigestCredential,
	#superclass : #ZnBasicCredential,
	#instVars : [
		'nonce',
		'opaque'
	],
	#category : #Zinc-HTTP-Support
}

{ #category : #accessing }
ZnDigestCredential class >> authName [
	^ 'Digest'
]

{ #category : #computation }
ZnDigestCredential >> a1 [
	^ ZnDigestAuthenticator md5Hash: self username, ':', self realm, ':', self password
]

{ #category : #computation }
ZnDigestCredential >> a2ForUrl: urlObject method: method [
	"This doesn't support all qop options yet."
	^ ZnDigestAuthenticator md5Hash: method, ':', (self uriPathFrom: urlObject)
]

{ #category : #accessing }
ZnDigestCredential >> authTokenForUrl: urlObject method: method [
	| result |
	result := WriteStream on: String new.
	result
		nextPutAll: 'Digest username="';
		nextPutAll: self username;
		nextPutAll: '", ';
		nextPutAll: 'nonce="';
		nextPutAll: self nonce;
		nextPutAll: '", ';
		nextPutAll: 'realm="';
		nextPutAll: self realm;
		nextPutAll: '", ';
		nextPutAll: 'uri="';
		nextPutAll: (self uriPathFrom: urlObject);
		nextPutAll: '", ';
		nextPutAll: 'response="';
		nextPutAll: (self responseForUrl: urlObject method: method);
		nextPutAll: '", '.
	self opaque isNil
		ifFalse: [ 
			result
				nextPutAll: 'opaque="';
				nextPutAll: self opaque;
				nextPutAll: '"' ].
	^ result contents
]

{ #category : #testing }
ZnDigestCredential >> isComplete [ 
	^ super isComplete & nonce notNil 
]

{ #category : #operations }
ZnDigestCredential >> linkToAuthRequest: aHeader [
	| data |
	data := self parseAuthRequest: aHeader.
	self nonce: (data at: 'nonce').
	self realm: (data at: 'realm' ifAbsent: [ '' ])
]

{ #category : #accessing }
ZnDigestCredential >> nonce [
	^ nonce
]

{ #category : #accessing }
ZnDigestCredential >> nonce: anObject [
	nonce := anObject
]

{ #category : #accessing }
ZnDigestCredential >> opaque [
	^ opaque
]

{ #category : #accessing }
ZnDigestCredential >> opaque: anObject [
	opaque := anObject
]

{ #category : #operations }
ZnDigestCredential >> parseAuthRequest: aHeader [
	^ ZnDigestAuthenticator parseAuthRequest: aHeader
]

{ #category : #computation }
ZnDigestCredential >> responseForUrl: urlObject method: method [
	^ ZnDigestAuthenticator md5Hash: self a1, ':', self nonce, ':', (self a2ForUrl: urlObject method: method)
]

{ #category : #operations }
ZnDigestCredential >> setAuthorizationHeader: headers for: urlObject method: method [
	headers at: 'Authorization' put: (self authTokenForUrl: urlObject method: method)
]

{ #category : #accessing }
ZnDigestCredential >> uriPathFrom: urlObject [
	^ urlObject pathPrintString ifNil: [ '/' ]
]

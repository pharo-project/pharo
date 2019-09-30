"
I am ZnDigestAuthenticator.
I help servers handle HTTP Digest Authentication.

I have a nonces dictionary with nonce->opaque associations. 
I generate a new nonce value for every 401 response I trigger.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnDigestAuthenticator,
	#superclass : #ZnBasicAuthenticator,
	#instVars : [
		'nonces'
	],
	#category : #'Zinc-HTTP-Support'
}

{ #category : #testing }
ZnDigestAuthenticator class >> hasMD5Support [
	^ (self md5Hash: 'test') isNil not
]

{ #category : #accessing }
ZnDigestAuthenticator class >> md5Hash: aString [
	| hash env|
	env := self environment.
	"Answer hash of aString as lowercase 32 digit hex String."
	env at: #MD5 ifPresent: [ :cls |
		^ ((cls new initialize) hashStream: (aString asByteArray readStream)) hex ].
	hash := env at: #CMD5Hasher ifPresent: [ :cls |
		cls hashMessage: aString ].
	hash ifNil: [
		hash := env at: #TCryptoRandom ifPresent: [ :cls |
			(cls basicNew perform: #md5HashMessage: with: aString) asInteger ] ].
	hash ifNotNil: [
		hash := hash hex asLowercase.
		(hash beginsWith: '16r') ifTrue: [ hash := hash allButFirst: 3 ].
		hash := hash padLeftTo: 32 with: $0 ].
	^ hash

]

{ #category : #accessing }
ZnDigestAuthenticator class >> parseAuthRequest: headerValue [
	| data dict |
	dict := Dictionary new.
	"Chops off the ' Digest ' scheme name, a bit too brittle."
	data := headerValue copyFrom: 8 to: headerValue size.
	(data substrings: ',') do: [ :fragment | | tokens i key value |
		tokens := fragment trimBoth.
		i := tokens indexOf: $=.
		(i > 0) ifTrue: [
			key := tokens copyFrom: 1 to: i - 1.
			value := tokens copyFrom: i + 1 to: tokens size.  
			dict at: key put: (value trimBoth: [ :char | char = $" ]) ] ].
	^ dict
]

{ #category : #private }
ZnDigestAuthenticator >> a1for: username [
	| password |
	password := credentials at: username ifAbsent: [ ^ nil ].
	^ self class md5Hash: username, ':', self realm, ':', password
	
]

{ #category : #private }
ZnDigestAuthenticator >> a2forUrl: uri method: method [
	^ self class md5Hash: method, ':', uri
	
]

{ #category : #public }
ZnDigestAuthenticator >> authHeader [
	| nonce opaque |
	nonce := self createNonce.
	opaque := self createOpaque.
	self nonces at: nonce put: opaque.
	^ 'Digest realm="', self realm, '", nonce="', nonce, '", opaque="', opaque, '"'
]

{ #category : #private }
ZnDigestAuthenticator >> createNonce [
	| pt |
	pt := DateAndTime now asString, ':', Random new next asString.
	^ self class md5Hash: pt
]

{ #category : #private }
ZnDigestAuthenticator >> createOpaque [
	| pt |
	pt := Random new next asString, ':', DateAndTime now asString.
	^ self class md5Hash: pt
]

{ #category : #testing }
ZnDigestAuthenticator >> isRequestAuthenticated: request [
	| authorization response cresponse a1 a2 nonce opaque |
	authorization := self class parseAuthRequest: (request headers at: 'Authorization' ifAbsent: [ ^ false ]).
	nonce := authorization at: 'nonce' ifAbsent: [ ^ false ].
	opaque := self nonces at: nonce ifAbsent: [ ^ false ].
	a1 := self a1for: (authorization at: 'username' ifAbsent: [ ^ false ]).
	a1 ifNil: [ ^ false ].
	a2 := self a2forUrl: (authorization at: 'uri' ifAbsent: [ '/' ]) method: request method.
	response := self class md5Hash: (a1, ':', nonce, ':', a2).
	"(opaque = (authorization at: 'opaque' ifAbsent: [^false])) ifFalse: [^false]."
	cresponse := authorization at: 'response' ifAbsent: [ ^ false ].
	^ (response = cresponse)
]

{ #category : #accessing }
ZnDigestAuthenticator >> nonces [ 
	^ nonces ifNil: [ nonces := Dictionary new ]
]

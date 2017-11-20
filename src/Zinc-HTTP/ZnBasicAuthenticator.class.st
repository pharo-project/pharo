"
I am ZnBasicAuthenticator.
I help servers handle HTTP Basic Authentication.
I implement the required #authenticateRequest:do:

I have a credentials dictionary with username->password associations.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnBasicAuthenticator,
	#superclass : #Object,
	#instVars : [
		'credentials',
		'realm'
	],
	#category : #Zinc-HTTP-Support
}

{ #category : #accessing }
ZnBasicAuthenticator class >> defaultRealm [
	^ 'ZincHTTPComponents'
]

{ #category : #'instance creation' }
ZnBasicAuthenticator class >> new [
	^ super new 
		initialize;
		yourself
]

{ #category : #'instance creation' }
ZnBasicAuthenticator class >> username: username password: password [
	^ self new
		atUsername: username putPassword: password;
		yourself
]

{ #category : #'instance creation' }
ZnBasicAuthenticator class >> username: username password: password realm: realm [
	^ self new
		atUsername: username putPassword: password;
		realm: realm;
		yourself
]

{ #category : #accessing }
ZnBasicAuthenticator >> atUsername: username putPassword: password [
	credentials at: username put: password
]

{ #category : #public }
ZnBasicAuthenticator >> authHeader [
	^ 'Basic realm=', self realm
]

{ #category : #public }
ZnBasicAuthenticator >> authenticateRequest: request do: block [
	^ (self isRequestAuthenticated: request)
		ifTrue: [ block value ]
		ifFalse: [ ZnResponse unauthorized: self authHeader ]
]

{ #category : #initialization }
ZnBasicAuthenticator >> initialize [
	credentials := Dictionary new
]

{ #category : #testing }
ZnBasicAuthenticator >> isRequestAuthenticated: request [
	| authorization username password |
	authorization := [ request basicAuthentication ] on: Error do: [ ^ false ].
	username := authorization first.
	password := authorization second.
	^ (credentials at: username ifAbsent: [ nil ]) = password
	
]

{ #category : #accessing }
ZnBasicAuthenticator >> realm [
	^ realm ifNil: [ realm := self class defaultRealm ]
]

{ #category : #accessing }
ZnBasicAuthenticator >> realm: anObject [
	realm := anObject
]

{ #category : #accessing }
ZnBasicAuthenticator >> removeAll [
	credentials removeAll
]

{ #category : #accessing }
ZnBasicAuthenticator >> removeUsername: username [
	credentials removeKey: username
]

"
I represent a session store for an HTTP client instance of ZnUserAgent. 

I hold the cookie jar for the session and any authentication credentials used by the client.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnUserAgentSession,
	#superclass : #Object,
	#instVars : [
		'cookiejar',
		'credentials',
		'currentRealm'
	],
	#category : 'Zinc-HTTP-Support'
}

{ #category : #'instance creation' }
ZnUserAgentSession class >> withCookieJar: aCookieJar [
	^ self new cookiejar: aCookieJar; yourself
]

{ #category : #accessing }
ZnUserAgentSession >> addCredential: aCredential [
	self credentials at: aCredential domain, '@', aCredential realm put: aCredential 
]

{ #category : #accessing }
ZnUserAgentSession >> cookieJar [
	^ cookiejar ifNil: [ cookiejar := ZnCookieJar new ]
]

{ #category : #accessing }
ZnUserAgentSession >> cookiejar: anObject [
	cookiejar := anObject
]

{ #category : #accessing }
ZnUserAgentSession >> credentialAt: aRealm forUrl: urlObject [
	^ self credentials at: urlObject authority, '@', aRealm ifAbsent: [ nil ]
]

{ #category : #accessing }
ZnUserAgentSession >> credentials [
	^ credentials ifNil: [ credentials := Dictionary new ]
]

{ #category : #accessing }
ZnUserAgentSession >> credentials: anObject [
	credentials := anObject
]

{ #category : #accessing }
ZnUserAgentSession >> credentialsForUrl: urlObject [
	^ self credentials values select: [ :cred | urlObject authority includesSubstring: (cred domain) ]
]

{ #category : #accessing }
ZnUserAgentSession >> currentRealm [
	^ currentRealm
]

{ #category : #accessing }
ZnUserAgentSession >> currentRealm: anObject [
	currentRealm := anObject
]

{ #category : #operations }
ZnUserAgentSession >> resetCookies [
	cookiejar ifNotNil: [ cookiejar removeAll ]
]

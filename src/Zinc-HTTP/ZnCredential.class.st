"
I am ZnCredential, an abstract class representing credentials in a domain and realm.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnCredential,
	#superclass : #Object,
	#instVars : [
		'domain',
		'realm'
	],
	#category : 'Zinc-HTTP-Support'
}

{ #category : #accessing }
ZnCredential class >> authName [
	self subclassResponsibility 
]

{ #category : #'accessing class hierarchy' }
ZnCredential class >> findHandlerFor: anAuthHeader [
	| type i |
	i := anAuthHeader indexOf: Character space.
	type := anAuthHeader copyFrom: 1 to: i - 1.
	self allSubclassesDo: [ :subclass |
		subclass authName = type ifTrue: [ ^ subclass ] ].
	^ nil
]

{ #category : #accessing }
ZnCredential class >> findRealmFor: anAuthHeader [
	(anAuthHeader substrings: ',') do: [ :token |
		"String>>trimBoth: is a grease method, so not sure if we should be using it."
		(token includesSubstring: 'realm=') 
			ifTrue: [ ^ ((token substrings: '=') second) ] ].
	^ nil
]

{ #category : #accessing }
ZnCredential >> authToken [
	^ self authTokenForUrl: nil method: nil 
]

{ #category : #accessing }
ZnCredential >> authTokenForUrl: urlObject method: method [
	self subclassResponsibility 
]

{ #category : #accessing }
ZnCredential >> domain [
	^ domain
]

{ #category : #accessing }
ZnCredential >> domain: anObject [
	domain := anObject
]

{ #category : #testing }
ZnCredential >> isComplete [
	^ self subclassResponsibility 
]

{ #category : #operations }
ZnCredential >> linkToAuthRequest: aHeader [
	"Link this credential to the WWW-Authenticate request in the provided header. 
	Used for authentication methods which require parameters provided by the server
	(e.g. Digest authentication)."
	realm := self class findRealmFor: aHeader
]

{ #category : #accessing }
ZnCredential >> realm [
	^ realm ifNil: [ realm := '' ]
]

{ #category : #accessing }
ZnCredential >> realm: anObject [
	realm := anObject
]

{ #category : #accessing }
ZnCredential >> setAuthorizationHeader: headers for: urlObject method: method [
	headers at: 'Authorization' put: self authToken
]

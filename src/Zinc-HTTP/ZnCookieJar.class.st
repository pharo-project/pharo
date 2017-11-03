"
I represent a container for storing HTTP Cookies for a client session.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnCookieJar,
	#superclass : #Object,
	#instVars : [
		'cookies'
	],
	#category : #Zinc-HTTP-Support
}

{ #category : #accessing }
ZnCookieJar >> add: aCookie [
	(self removeIfExpired: aCookie) ifTrue: [ ^ nil ].
	self cookies do: [ :cookie |
		cookie = aCookie ifTrue: [ self cookies remove: cookie ] ].
	self cookies add: aCookie
	
]

{ #category : #accessing }
ZnCookieJar >> cookieAt: aName forUrl: urlObject [
	(self cookiesForUrl: urlObject) do: [ :cookie | cookie name = aName ifTrue: [ ^ cookie ] ].
	^ nil
]

{ #category : #accessing }
ZnCookieJar >> cookies [ 
	^ cookies ifNil: [ cookies := Bag new ]
]

{ #category : #accessing }
ZnCookieJar >> cookiesForUrl: urlObject [
	^ self cookies select: [ :cookie | 
		(cookie isExpired not) & (cookie isInDomain: urlObject) & (cookie isInPath: urlObject) ]
]

{ #category : #accessing }
ZnCookieJar >> remove: aCookie [
	self cookies remove: aCookie ifAbsent: [ nil ]
]

{ #category : #operations }
ZnCookieJar >> removeAll [
	cookies ifNotNil: [ cookies removeAll ]
]

{ #category : #operations }
ZnCookieJar >> removeIfExpired: aCookie [
	^ aCookie isExpired 
		ifTrue: [ 
			self remove: aCookie. 
			true ]
		ifFalse: [
			false ]
]

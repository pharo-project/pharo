"
I represent a container for storing HTTP Cookies for a client session.

Part of Zinc HTTP Components.
"
Class {
	#name : 'ZnCookieJar',
	#superclass : 'Object',
	#instVars : [
		'cookies'
	],
	#category : 'Zinc-HTTP-Support',
	#package : 'Zinc-HTTP',
	#tag : 'Support'
}

{ #category : 'accessing' }
ZnCookieJar >> add: aCookie [
	(self removeIfExpired: aCookie) ifTrue: [ ^ nil ].
	self cookies do: [ :cookie |
		cookie = aCookie ifTrue: [ self cookies remove: cookie ] ].
	self cookies add: aCookie
]

{ #category : 'accessing' }
ZnCookieJar >> cookieAt: aName forUrl: urlObject [
	(self cookiesForUrl: urlObject) do: [ :cookie | cookie name = aName ifTrue: [ ^ cookie ] ].
	^ nil
]

{ #category : 'accessing' }
ZnCookieJar >> cookies [
	^ cookies ifNil: [ cookies := Bag new ]
]

{ #category : 'accessing' }
ZnCookieJar >> cookiesForUrl: urlObject [
	^ self cookies select: [ :cookie |
		(cookie isExpired not) & (cookie isInDomain: urlObject) & (cookie isInPath: urlObject) ]
]

{ #category : 'operations' }
ZnCookieJar >> readNetscapeFormatFrom: fileReference [
	| cookie |
	fileReference readStreamDo: [ :fileStream | 
		(ZnFastLineReader on: fileStream) linesDo: [ :each | 
			(each beginsWith: '#')
				ifFalse: [ cookie := ZnCookie new.
					cookie fromNetscapeString: each.
					self add: cookie ] ] ]
]

{ #category : 'accessing' }
ZnCookieJar >> remove: aCookie [
	self cookies remove: aCookie ifAbsent: [ nil ]
]

{ #category : 'operations' }
ZnCookieJar >> removeAll [
	cookies ifNotNil: [ cookies removeAll ]
]

{ #category : 'operations' }
ZnCookieJar >> removeIfExpired: aCookie [
	^ aCookie isExpired
		ifTrue: [
			self remove: aCookie.
			true ]
		ifFalse: [
			false ]
]

{ #category : 'operations' }
ZnCookieJar >> writeNetscapeFormatTo: fileReference [
	fileReference writeStreamDo: [ :fileStream |
		(ZnNewLineWriterStream on: fileStream) in: [ :out |
			out nextPutAll: '# Netscape HTTP Cookie File'; cr.
			self cookies do: [ :each |
				out nextPutAll: each netscapeString; cr ] ] ]
]

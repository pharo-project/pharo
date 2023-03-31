"
ZnUrlShortenerDelegateTest is a functional test for ZnUrlShortenerDelegate
"
Class {
	#name : #ZnUrlShortnerDelegateTest,
	#superclass : #TestCase,
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #tests }
ZnUrlShortnerDelegateTest >> testCreate [
	| url |
	url := ('https://pharo.org/tst-' , 9999 atRandom asString) asUrl.
	self withServerDo: [ :server | | client shortUrl |
		client := ZnClient new url: server url; queryAt: #url put: url.
		client get.
		shortUrl := client response contents.
		client followRedirects: false.
		client get: shortUrl.
		self assert: client response isRedirect.
		self assert: client response location asUrl equals: url.
		client close ]
]

{ #category : #tests }
ZnUrlShortnerDelegateTest >> testDatabase [
	| url key |
	url := 'https://pharo.org' asUrl.
	key := ZnUrlShortenerDelegate shorten: url.
	self
		assert: (ZnUrlShortenerDelegate database at: key)
		equals: url.
]

{ #category : #tests }
ZnUrlShortnerDelegateTest >> testRedirect [
	| url key |
	url := 'https://pharo.org' asUrl.
	key := ZnUrlShortenerDelegate shorten: url.
	self withServerDo: [ :server | | client |
		client := ZnClient new url: server url; addPathSegment: key.
		client followRedirects: false.
		client get.
		self assert: client response isRedirect.
		self assert: client response location asUrl equals: url.
		client close ]
]

{ #category : #enumerating }
ZnUrlShortnerDelegateTest >> withServerDo: block [
	ZnServer withOSAssignedPortDo: [ :server |
		server delegate: ZnUrlShortenerDelegate new.
		block cull: server ]
]

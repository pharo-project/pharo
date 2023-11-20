Class {
	#name : 'ZnUserAgentSessionTest',
	#superclass : 'TestCase',
	#category : 'Zinc-Tests',
	#package : 'Zinc-Tests'
}

{ #category : 'accessing' }
ZnUserAgentSessionTest >> dummyCredential [
	^ZnBasicCredential new
		username: 'test';
		password: 'foobar';
		realm: 'dummy';
		domain: 'test.com';
		yourself
]

{ #category : 'testing' }
ZnUserAgentSessionTest >> testAccessors [
	| session |
	session := ZnUserAgentSession new.
	self assert: (session cookieJar isKindOf: ZnCookieJar).
	self assert: (session credentials isNotNil)
]

{ #category : 'testing' }
ZnUserAgentSessionTest >> testAddCredential [
	| session count |
	session := ZnUserAgentSession new.
	count := session credentials size.
	session addCredential: self dummyCredential.
	self assert: session credentials size equals: count + 1
]

{ #category : 'testing' }
ZnUserAgentSessionTest >> testWithCookieJar [
	self assert: ((ZnUserAgentSession withCookieJar: ZnCookieJar new) isNotNil)
]

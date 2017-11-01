Class {
	#name : #ZnUserAgentSessionTests,
	#superclass : #TestCase,
	#category : 'Zinc-Tests'
}

{ #category : #accessing }
ZnUserAgentSessionTests >> dummyCredential [
	^ZnBasicCredential new 
		username: 'test'; 
		password: 'foobar';
		realm: 'dummy';
		domain: 'test.com';
		yourself
]

{ #category : #testing }
ZnUserAgentSessionTests >> testAccessors [
	| session |
	session := ZnUserAgentSession new.
	self assert: (session cookieJar isKindOf: ZnCookieJar).
	self assert: (session credentials notNil)
]

{ #category : #testing }
ZnUserAgentSessionTests >> testAddCredential [
	| session count |
	session := ZnUserAgentSession new.
	count := session credentials size.
	session addCredential: self dummyCredential.
	self assert: ((session credentials size) = (count + 1))
]

{ #category : #testing }
ZnUserAgentSessionTests >> testWithCookieJar [
	self assert: ((ZnUserAgentSession withCookieJar: ZnCookieJar new) notNil)
]

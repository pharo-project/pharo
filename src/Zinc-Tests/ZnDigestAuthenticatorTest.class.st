Class {
	#name : #ZnDigestAuthenticatorTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnDigestAuthenticatorTest >> testParseEmpty [
	| keys headerValue |
	headerValue := ' Digest username="Mufasa",
                     realm="testrealm@host.com",
                     nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093",
                     uri="/dir/index.html",
                     qop=,
                     nc="",
                     cnonce="0a4f113b",
                     response="6629fae49393a05397450978507c4ef1",
                     opaque="5ccc069c403ebaf9f0171e9517f40e41"'.
	keys := ZnDigestAuthenticator parseAuthRequest: headerValue.
	self assert: (keys at: 'username') equals: 'Mufasa'.
	self assert: (keys at: 'realm') equals: 'testrealm@host.com'.
	self assert: (keys at: 'qop') equals: ''.
	self assert: (keys at: 'nc') equals: ''.
	self assert: (keys at: 'cnonce') equals: '0a4f113b'.
	self assert: (keys at: 'response') equals: '6629fae49393a05397450978507c4ef1'.
	
]

{ #category : #testing }
ZnDigestAuthenticatorTest >> testParseOK [
	| keys headerValue |
	headerValue := ' Digest username="Mufasa",
                     realm="testrealm@host.com",
                     nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093",
                     uri="/dir/index.html",
                     qop=auth,
                     nc=00000001,
                     cnonce="0a4f113b",
                     response="6629fae49393a05397450978507c4ef1",
                     opaque="5ccc069c403ebaf9f0171e9517f40e41"'.
	keys := ZnDigestAuthenticator parseAuthRequest: headerValue.
	self assert: (keys at: 'username') equals: 'Mufasa'.
	self assert: (keys at: 'realm') equals: 'testrealm@host.com'.
	self assert: (keys at: 'qop') equals: 'auth'.
	self assert: (keys at: 'nc') equals: '00000001'.
	self assert: (keys at: 'cnonce') equals: '0a4f113b'.
	self assert: (keys at: 'response') equals: '6629fae49393a05397450978507c4ef1'.
	
]

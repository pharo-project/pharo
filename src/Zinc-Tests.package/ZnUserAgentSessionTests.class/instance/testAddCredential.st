testing
testAddCredential
	| session count |
	session := ZnUserAgentSession new.
	count := session credentials size.
	session addCredential: self dummyCredential.
	self assert: ((session credentials size) = (count + 1))
testing
testDefaultPortUnknownScheme
	| url |
	url := 'gitfiletree://github.com/rohitsukhwal/HelloWorld.git?protocol=git' asZnUrl.
	self assert: url scheme equals: #gitfiletree.
	self assert: url host equals: 'github.com'.
	self assert: url authority equals: 'github.com'.
	self assert: url portOrDefault isNil.
	self deny: url hasPort.
	self assert: url equals: url.
	self should: [ url enforceKnownScheme ] raise: ZnUnknownScheme 
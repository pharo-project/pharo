testing
testNoSchemeColonInPath
	| url |
	url := '/foo:bar' asZnUrl.
	self deny: url hasScheme.
	self deny: url hasHost.
	self deny: url hasPort.
	self deny: url hasQuery.
	self deny: url hasFragment.
	self assert: url hasPath.
	self assert: url firstPathSegment equals: 'foo:bar'
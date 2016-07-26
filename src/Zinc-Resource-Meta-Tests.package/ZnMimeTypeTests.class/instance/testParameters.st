testing
testParameters
	| mimeType |
	mimeType := ZnMimeType main: 'text' sub: 'plain'.
	self should: [ mimeType parameterAt: 'foo' ] raise: KeyNotFound.
	self assert: (mimeType parameterAt: 'foo' ifAbsent: [ #none ]) = #none.
	mimeType parameterAt: 'foo' put: '1'.
	self assert: (mimeType parameterAt: 'foo') = '1'.
	self assert: (mimeType parameterAt: 'foo' ifAbsent: [ #none ]) = '1'.
	mimeType removeParameter: 'foo'.
	mimeType removeParameter: 'bar'.
	self should: [ mimeType parameterAt: 'foo' ] raise: KeyNotFound.
	self assert: (mimeType parameterAt: 'foo' ifAbsent: [ #none ]) = #none	
tests
testIndicator
	"self debug: #testIndicator"
	
	| ref |
	ref := filesystem * 'plonk' / 'griffle'.
	self deny: ref exists.
	self assert: ref indicator = '?'.
	
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	[ref createDirectory.
	 self assert: ref exists.
	self assert: ref isDirectory.
	 self assert: ref indicator equals: '/' ] ensure: [ref delete].
	
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	[ref writeStreamDo: [:stream | stream nextPutAll: 'foo'  ] ifPresent: [self fail].
	 self assert: ref exists.
	self assert: ref isFile.
	 self assert: ref indicator equals: '' ] ensure: [ref delete].
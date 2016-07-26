tests
testExtractUpdatesForVersion
	"self debug: #testExtractUpdatesForVersion"
	
	| res |
	res := UpdateStreamer new extractVersion: 'Pharo1.1a' from: self miniUpdate.
	self assert: (res size = 13).
	res := UpdateStreamer new extractVersion: 'Pharo1.2a' from: self miniUpdate.
	self assert: (res size = 6).
	res := UpdateStreamer new extractVersion: 'ZORK.2a' from: self miniUpdate.
	self assert: (res isEmpty).
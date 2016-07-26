tests
testExtractUpdatesForVersionLf
	"self debug: #testExtractUpdatesForVersionLf"
	
	| res |
	res := UpdateStreamer new extractVersion: 'Pharo1.1a' from: self miniUpdateLf.
	self assert: (res size = 13).
	res := UpdateStreamer new extractVersion: 'Pharo1.2a' from: self miniUpdateLf.
	self assert: (res size = 6).
	res := UpdateStreamer new extractVersion: 'ZORK.2a' from: self miniUpdateLf.
	self assert: (res isEmpty).
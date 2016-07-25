testing
testWritingBinary
	| response bytes string |
	response := ZnResponse ok: (ZnEntity text: 'Okay').
	bytes := ByteArray streamContents: [ :stream | response writeOn: stream ].
	string := bytes asString.
	self assert: (string includesSubstring: '200 OK').
	self assert: (string includesSubstring: 'Okay')
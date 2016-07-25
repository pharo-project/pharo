testing
testWritingCharacter
	| response string |
	response := ZnResponse ok: (ZnEntity text: 'Okay').
	string := String streamContents: [ :stream | response writeOn: stream ].
	self assert: (string includesSubstring: '200 OK').
	self assert: (string includesSubstring: 'Okay')
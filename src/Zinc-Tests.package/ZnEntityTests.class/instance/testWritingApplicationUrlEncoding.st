testing
testWritingApplicationUrlEncoding
	| entity string |
	entity := ZnApplicationFormUrlEncodedEntity new.
	entity at: 'foo' put: 'bar'.
	string := String streamContents: [ :stream | entity writeOn: stream ].
	self assert: string = 'foo=bar'.
	self assert: entity contentLength = 7
testing
testWritingApplicationUrlEncodingWithTextEncodingUtf8
	| entity string |
	entity := ZnApplicationFormUrlEncodedEntity new.
	entity contentType charSet: 'utf-8'.
	entity at: 'foo' put: (Character value: 246) asString.
	string := String streamContents: [ :stream | entity writeOn: stream ].
	self assert: string = 'foo=%C3%B6'.
	self assert: entity contentLength = 10
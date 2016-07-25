testing
testWriting
	| headers string |
	headers := ZnHeaders new 
		at: 'Content-Type' put: 'text/plain'; 
		at: 'Content-Length' put: '128';
		yourself.
	string := String streamContents: [ :stream | headers writeOn: stream ].
	self assert: (string includesSubstring: 'Content-Type: text/plain', String crlf).
	self assert: (string includesSubstring: 'Content-Length: 128', String crlf).
	self assert: (string occurrencesOf: Character cr) = 2.
	self assert: (string occurrencesOf: Character lf) = 2
testing
testReading
	| string headers |
	string := 'Content-Type: text/plain', String crlf, 'Content-Length: 128', String crlf.
	headers := ZnHeaders readFrom: string asByteArray readStream.
	self assert: (headers includesKey: 'Content-Type').
	self assert: (headers includesKey: 'Content-Length').
	self assert: (headers at: 'Content-Type') equals: 'text/plain'.
	self assert: (headers at: 'Content-Length') equals: '128'.
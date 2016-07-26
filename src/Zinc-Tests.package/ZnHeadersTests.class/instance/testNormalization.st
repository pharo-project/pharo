testing
testNormalization
	| headers |
	headers := ZnHeaders new.
	headers at: 'content-TYPE' put: 'text/plain'.
	self assert: (headers includesKey: 'content-type').
	self assert: (headers includesKey: 'content-TYPE').
	self assert: (headers at: 'Content-Type') equals: 'text/plain'.
	headers at: 'CONTENT-type' put: 'text/html'.
	self assert: (headers at: 'Content-Type') equals: 'text/html'.

	
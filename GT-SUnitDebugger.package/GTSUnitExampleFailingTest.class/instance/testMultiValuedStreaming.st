tests
testMultiValuedStreaming
	| headers keysAndValues |
	headers := ZnHeaders new.
	headers at: 'Set-Cookie' put: 'foo=1'.
	headers at: 'Set-Cookie' add: 'bar=1'.
	headers at: 'Content-TYPE' put: 'text/plain'.
	headers at: 'Content-Length' put: '128'.
	keysAndValues := OrderedCollection streamContents: [ : stream |
		headers headersDo: [ :key :value |
			stream nextPut: key; nextPut: value ] ].
	self assert: keysAndValues equals: #('Set-Cookie' 'foo=1' 'Set-Cookie' 'bar=1' 'Content-Length' '128' 'Content-TYPE' 'text/plain') asOrderedCollection 
	
	
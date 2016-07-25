accessing
headers
	headers isNil ifTrue: [ headers := ZnHeaders new ].
	^ headers
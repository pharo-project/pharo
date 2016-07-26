printing-numerative
printStringRoman
	| stream integer |
	stream := String new writeStream.
	integer := self negative ifTrue: [stream nextPut: $-. self negated] ifFalse: [self].
	integer // 1000 timesRepeat: [stream nextPut: $M].
	integer
		romanDigits: 'MDC' for: 100 on: stream;
		romanDigits: 'CLX' for: 10 on: stream;
		romanDigits: 'XVI' for: 1 on: stream.
	^stream contents
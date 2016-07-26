accessing
crlf
	CrLf ifNil: [CrLf := self with: Character cr with: Character lf].
	^CrLf
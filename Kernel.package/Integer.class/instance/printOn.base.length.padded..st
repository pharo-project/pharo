printing-numerative
printOn: aStream base: base length: minimum padded: zeroFlag
	| prefix |
	
	prefix := self negative ifTrue: ['-'] ifFalse: [ '' ].
	
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag

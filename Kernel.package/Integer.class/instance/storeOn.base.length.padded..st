printing-numerative
storeOn: aStream base: base length: minimum padded: zeroFlag
	| prefix |
	prefix := self negative ifTrue: ['-'] ifFalse: [String new].
	base = 10 ifFalse: [prefix := prefix, base printString, 'r'].
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag

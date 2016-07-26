printing-numerative
storeOn: aStream base: base
	"Print a representation of the receiver on the stream
	<aStream> in base <base> where
	2 <= <baseInteger> <= 16. If <base> is other than 10
	it is written first separated by $r followed by the number
	like for example: 16rFCE2"

	| integer |
	integer := self negative
		ifTrue: [aStream nextPut: $-. self negated]
		ifFalse: [self].
	base = 10 ifFalse: [aStream nextPutAll: base printString; nextPut: $r].
	aStream nextPutAll: (integer printStringBase: base).

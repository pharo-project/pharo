printing
printOn: aStream withLeadingSpace: printLeadingSpaceToo
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1.
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	nanos ~= 0 
		ifTrue: [ | n len |
			n := nanos. len := 9.
			[ n \\ 10 = 0 ] whileTrue: [ n := n / 10. len := len - 1 ].
			aStream nextPut: $..
			n printOn: aStream base: 10 length: len padded: true ].
	aStream nextPut: (offset positive ifTrue: [ $+ ] ifFalse: [ $- ]).
	offset hours abs printOn: aStream base: 10 length: 2 padded: true.
	aStream nextPut: $:.
	offset minutes abs printOn: aStream base: 10 length: 2 padded: true.
	offset seconds = 0 
		ifFalse:[ 
			aStream nextPut: $:; print: offset seconds abs truncated ]
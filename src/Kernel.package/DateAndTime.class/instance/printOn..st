printing
printOn: aStream
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1.
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)"

	^self printOn: aStream withLeadingSpace: false

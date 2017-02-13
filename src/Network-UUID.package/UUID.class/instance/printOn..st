printing
printOn: aStream
	"Print my official representation, 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens) on aStream"

	1 to: 4 do: [ :i | self printHexAt: i to: aStream ].
	aStream nextPut: $-.
	5 to: 6 do: [ :i | self printHexAt: i to: aStream ].
	aStream nextPut: $-.
	7 to: 8 do: [ :i | self printHexAt: i to: aStream ].
	aStream nextPut: $-.
	9 to: 10 do: [ :i | self printHexAt: i to: aStream ].
	aStream nextPut: $-.
	11 to: 16 do: [ :i | self printHexAt: i to: aStream ]
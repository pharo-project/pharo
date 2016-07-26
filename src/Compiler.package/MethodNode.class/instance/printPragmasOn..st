printing
printPragmasOn: aStream
	properties ifNil: [^self].
	properties pragmas do:
		[:pragma|
		"Primitives are printed in printPrimitiveOn:; skip these"
		(Parser primitivePragmaSelectors includes: pragma keyword) ifFalse:
			[aStream crtab: 1.
			 pragma printOn: aStream]]
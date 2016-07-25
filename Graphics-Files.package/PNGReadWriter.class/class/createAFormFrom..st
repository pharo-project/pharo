instance creation
createAFormFrom: data 
	| error f |
	error := ''.
	f := [ self formFromStream: (RWBinaryOrTextStream with: data) ] ifError: 
		[ :a :b | 
		error := a printString , '  ' , b printString.
		(StringMorph contents: error)
			color: Color red;
			imageForm ].
	^ {  f. error  }
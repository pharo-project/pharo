printing
printElementsOn: stream
	stream nextPut: $#; print: self size.
	stream space; print: weight total; nextPut: $/; print: weight maximum.
	stream space; print: weight compute.
	factory ifNotNil: [ stream space; print: factory ].
	stream space; print: (self hitRatio * 100.0) rounded ; nextPut: $%
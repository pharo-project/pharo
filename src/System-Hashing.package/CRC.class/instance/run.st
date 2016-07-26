public
run
	lookupTable ifNil: [ lookupTable := self generateLookupTable ].
	^ self perform: self runMethodSelector
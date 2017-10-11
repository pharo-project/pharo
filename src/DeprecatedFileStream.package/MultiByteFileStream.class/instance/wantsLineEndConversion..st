remnant
wantsLineEndConversion: aBoolean
	
	wantsLineEndConversion :=  aBoolean.

	lineEndConvention ifNil: [ self detectLineEndConvention ]. 
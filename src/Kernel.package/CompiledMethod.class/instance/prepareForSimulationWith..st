debugger support
prepareForSimulationWith: numArgs
	"This method changes the argument count of a CompiledMethod header to numArgs, its temporary count to numArgs + 1 and change the code handling primitive error to store the error code in the unique temporary of the method"
			
	| newHeader |
	newHeader := (((self header bitAnd: 2r01110000000000111111111111111111) 
			bitOr: (numArgs bitShift: 24))
			bitOr: (numArgs + 1 bitShift: 18)).
	newHeader := newHeader + (self class headerFlagForEncoder: self encoderClass).
	self objectAt: 1 put: newHeader.
	
	self encoderClass prepareMethod: self forSimulationWith: numArgs
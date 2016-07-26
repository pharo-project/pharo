*Fuel
fuelPrepare
	"Prepare for Fuel serialization."
	
	self isNamedPrimitive ifTrue: [self literals first at: 4 put: 0].
	
	"When the method is a named primitive, the first literal is an array that works as an argument for the VM. The first and second elements represent the module and the name of the primivite. The third element of the array is ignored, it used to be a session ID. The forth element is the primitive index in a externalPrimitiveTable and it is IMPORTANT to put it in zero, otherwise there can be errors. Putting a zero means that the VM will not try to directly execute the primitive from the table and it will always try to load the module. It is the safer thing to do. For more details, read the method #primitiveExternalCall in VMMaker."
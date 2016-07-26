accessing-implementations
beCrc16
	self
		width: 16;
		lookupTable: self class crc16Table;
		polynome: 16r8005;
		registerFill: 16r0;
		reflectInput: true;
		reflectOutput: true;
		finallyXorWith: 16r0
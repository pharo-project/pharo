accessing-implementations
beCrc32
	self
		width: 32;
		lookupTable: self class crc32Table;
		polynome: 16r04C11DB7;
		registerFill: 16rFFFFFFFF;
		reflectInput: true;
		reflectOutput: true;
		finallyXorWith: 16rFFFFFFFF
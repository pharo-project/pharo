*Fuel
serializeOn: anEncoder

	anEncoder 
		encodeUint32: (self at: 1);
		encodeUint32: (self at: 2).
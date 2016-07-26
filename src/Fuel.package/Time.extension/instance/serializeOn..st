*Fuel
serializeOn: anEncoder

	anEncoder 
		encodeUint24: self asSeconds;
		encodeUint32: self nanoSecond.
		
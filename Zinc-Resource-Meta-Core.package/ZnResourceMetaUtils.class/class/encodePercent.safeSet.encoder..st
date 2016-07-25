conversions
encodePercent: string safeSet: safeSet encoder: encoder
	^ ZnPercentEncoder new
		characterEncoder: encoder;
		safeSet: (self perform: safeSet);
		encode: string
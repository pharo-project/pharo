conversions
decodePercent: string
	^ ZnPercentEncoder new 
		decodePlusAsSpace: false; 
		decode: string
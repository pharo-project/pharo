conversions
decodePercentForQuery: string
	^ ZnPercentEncoder new 
		decodePlusAsSpace: true; 
		decode: string
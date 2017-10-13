accessing
finalHash
	"Concatenate the final totals to build the 256-bit integer result."
	"Details: If the primitives are supported, the results are in the totals array. Otherwise, they are in the instance variables totalA through totalE."

		^ (totalA asInteger bitShift: 224) +
		  (totalB asInteger bitShift: 192) +
		  (totalC asInteger bitShift: 160) +	
		  (totalD asInteger bitShift: 128) +
		  (totalE asInteger bitShift:  96) +
		  (totalF asInteger bitShift:  64) +
		  (totalG asInteger bitShift:  32) +
		  (totalH asInteger).
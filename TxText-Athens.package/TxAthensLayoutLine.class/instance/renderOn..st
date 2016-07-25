rendering
renderOn: aTxTextRenderer 
	
	"aTxTextRenderer canvas 
		setPaint: (Color red alpha:0.5);
		drawShape: (0@ maxAscent negated corner: width@(maxAscent negated+height)).
"	
	commands do: [:cmd | cmd renderOn: aTxTextRenderer ]
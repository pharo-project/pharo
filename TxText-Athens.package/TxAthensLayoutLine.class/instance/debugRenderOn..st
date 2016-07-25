rendering
debugRenderOn: aTxTextRenderer 
	
	aTxTextRenderer canvas 
		setPaint: Color red;
		drawShape: (0@ maxAscent negated corner: width@(maxAscent negated+height)).
	
	commands do: [:cmd | cmd renderOn: aTxTextRenderer ]
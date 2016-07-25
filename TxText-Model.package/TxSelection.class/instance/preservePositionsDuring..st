helpers
preservePositionsDuring: aBlock
	"Restore start and end after evaluating a block, using slow text index calculation"
	
	| startPos endPos |
	
	startPos := start asTextIndex.
	endPos := end asTextIndex.
	
	aBlock ensure: [
		start position: (self text positionAtTextIndex: startPos).
		end position: (self text positionAtTextIndex: endPos).
	]. 
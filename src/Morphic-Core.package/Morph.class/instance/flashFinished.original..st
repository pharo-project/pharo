macpal
flashFinished: flashFill original: originalFill

	"if the color/fillstyle has changed while the flash is active, it will continue to use the new color/fill and not restrore the original"
	
	self fillStyle == flashFill ifTrue: [ 
		self fillStyle: originalFill	
	]
actions
updateDescriptionFillStyle: aColor
	"Change the colours on the descriptions."
	
	self descriptionMorph fillStyle: aColor.
	self fromDescriptionMorph owner color: (aColor alpha: 0.5).
	self toDescriptionMorph owner color: (aColor alpha: 0.3).
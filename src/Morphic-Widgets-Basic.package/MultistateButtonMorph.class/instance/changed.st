updating
changed
	"Update the fillStyle here."
	
	self assureExtension.
	extension fillStyle: self fillStyleToUse.
	color := self fillStyle asColor.
	super changed
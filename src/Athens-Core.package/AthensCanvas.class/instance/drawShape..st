drawing
drawShape: anObject
	"A convenience method, which sets the current shape to anObject and then fills it
	with currently selected paint" 
	self setShape: anObject.
	self draw.
	
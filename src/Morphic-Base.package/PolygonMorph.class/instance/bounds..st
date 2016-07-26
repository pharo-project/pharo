geometry
bounds: newBounds
	"This method has to be reimplemented since self extent: will also change self bounds origin,
	super bounds would leave me in wrong position when container is growing.
	Always change extent first then position"
	
	self extent: newBounds extent; position: newBounds topLeft

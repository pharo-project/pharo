private
newCanvas
	"Answer a preinitialized instance of AthensCanvas.
	Private to receiver and its subclasses, override seldom"
	
	^ self primCreateCanvas surface: self


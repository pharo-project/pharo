instance creation
new: nElements
	"Create a Set large enough to hold nElements without growing"
	^ self basicNew initialize: (self sizeFor: nElements)
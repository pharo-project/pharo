adding/removing
addClassNamed: className 
	"Creates a class with the given name"
	self addClass: (RGClassDefinition named: className)
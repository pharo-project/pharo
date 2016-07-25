query
createMethodNamed: aSelector realParent: class

	"This method has some problems."
	"1 - It should use the object that comes from the browsed environment"
	"2 - It does not create a method but a representation of it so be careful"
	^ (RGMethodDefinition realClass: class selector: aSelector) package:  (class>>aSelector) package asRingDefinition; yourself .
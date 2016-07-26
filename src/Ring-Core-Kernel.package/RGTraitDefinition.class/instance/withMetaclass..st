behavior
withMetaclass: aRGMetatraitDefinition
	"Registers explicitly the metaclass of a class"

	metaClass:= aRGMetatraitDefinition.
	metaClass baseClass: self.  

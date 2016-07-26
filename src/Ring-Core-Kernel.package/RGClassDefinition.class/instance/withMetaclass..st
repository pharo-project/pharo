behavior
withMetaclass: aRGMetaclassDefinition
	"Registers explicitly the metaclass of a class"

	metaClass:= aRGMetaclassDefinition.
	metaClass baseClass: self.  

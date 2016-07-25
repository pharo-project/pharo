class addition
addClassDefinition: aClass
	"Add only the class definition of aClass, as a class defined by the receiver. The term classdefinition denotes the fact that we do not treat its methods. aClass methods are not added to the package. aClass can be either a class or a metaclass"
	
	self addClassDefinitionName: aClass theNonMetaClass name.
	
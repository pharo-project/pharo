class addition
removeClassDefinition: aClass
	"Remove the class definition from the package but not its methods."
	^ self removeClassDefinitionName: aClass theNonMetaClass originalName.
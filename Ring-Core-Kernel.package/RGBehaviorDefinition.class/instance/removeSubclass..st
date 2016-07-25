subclassing
removeSubclass: aRGBehaviorDefinition 
	"Removes aRGAbstractClassDefinition from the direct subclasses - without failing if does not exist"
	
	self subclasses remove: aRGBehaviorDefinition ifAbsent: []
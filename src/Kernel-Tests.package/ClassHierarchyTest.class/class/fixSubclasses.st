fixing
fixSubclasses
	"Fix all the missing subclasses"
	"ClassHierarchyTest fixSubclasses "
	SystemNavigation new allClassesDo: [ :cls|
		(cls superclass subclasses includes: cls)
			ifFalse: [ cls superclass addSubclass: cls ]]
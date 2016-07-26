menus behavior
findSuperclass
	"Search for a superclass of the selected class from a pattern or from the recent list"
	self selectedClass ifNotNil: [ :class| 
		self findClass: (SearchFacade superclassSearchFor: class theNonMetaClass)].
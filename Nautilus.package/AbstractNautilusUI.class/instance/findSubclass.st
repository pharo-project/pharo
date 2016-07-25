menus behavior
findSubclass
	"Search for a subclass of the selected class from a pattern or from the recent list"
	
	self selectedClass ifNotNil: [ :class| 
		self findClass: (SearchFacade subclassSearchFor: class theNonMetaClass)].
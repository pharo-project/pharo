menus behavior
showUnreferencedClassVars
	"Search for all class variables known to the selected class, and put up a 
	list of those that have no references anywhere in the system. The 
	search includes superclasses, so that you don't need to navigate your 
	way to the class that defines each class variable in order to determine 
	whether it is unreferenced"
	self showUnreferencedClassVarsOf: self selectedClass.
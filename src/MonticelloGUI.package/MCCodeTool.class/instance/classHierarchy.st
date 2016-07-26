menus
classHierarchy
	"Create and schedule a class list browser on the receiver's hierarchy."

	self systemNavigation 
		browseHierarchy: self selectedClassOrMetaClass
		selector: self selectedMessageName	"OK if nil"
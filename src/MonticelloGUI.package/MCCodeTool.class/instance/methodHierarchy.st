menus
methodHierarchy
	"Create and schedule a method browser on the hierarchy of implementors."

	self systemNavigation methodHierarchyBrowserForClass: self selectedClassOrMetaClass
		selector: self selectedMessageName
actions browsing
methodHierarchy
	"Create and schedule a method browser on the hierarchy of implementors."

	self systemNavigation 
			methodHierarchyBrowserForClass: self currentClassOrMetaClass 
			selector: self currentMessageName

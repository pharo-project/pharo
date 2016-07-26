menus behavior
findClass
	"Search for a class from a pattern or from the recent list"
	self findClass: (SearchFacade classSearchInEnvironment: self browsedEnvironment).
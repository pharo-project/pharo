plugins announcements
openSubclass
	"Search for a superclass of the selected class from a pattern or from the recent list"

	self selectedClass ifNotNil: [ :class| | search |
		search := SearchFacade subclassSearchFor: class theNonMetaClass.
		self openClass: (search chooseFromOwner: self window) ].
plugins announcements
openSuperclass
	"Search for a superclass of the selected class from a pattern or from the recent list"

	self selectedClass ifNotNil: [ :class| 
		self openClass: class superclass ].
menu - message
browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	self selectedClass ifNotNil:
		[:myClass | Smalltalk tools browser openOnClass: myClass selector: self selectedSelector ]
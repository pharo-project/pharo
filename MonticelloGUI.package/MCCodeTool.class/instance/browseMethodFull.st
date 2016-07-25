menus
browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	| myClass |
	(myClass := self selectedClassOrMetaClass) ifNotNil:
		[Smalltalk tools browser openOnClass: myClass selector: self selectedMessageName]
menu messages
classNamesContainingIt
	"Open a browser on classes whose names contain the selected string"

	self lineSelectAndEmptyCheck: [^self].
	model systemNavigation 
		browseClassesWithNamesContaining: self selection string
		caseSensitive: Sensor shiftPressed
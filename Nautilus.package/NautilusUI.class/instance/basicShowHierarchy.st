private
basicShowHierarchy
	"Prepares the class panel to show hierarchies"
	
	self selectedClass ifNil: [ ^ self ].	"Since the double clic comes after a simple clic and simple clic deselects selected item, we are forced to have this guard. A alternative would be to change the simple clic but the side effects are too big"
	self resetClassesListSelection.
	self classWidget selectClass: self selectedClass.
	self resetClassesList.	
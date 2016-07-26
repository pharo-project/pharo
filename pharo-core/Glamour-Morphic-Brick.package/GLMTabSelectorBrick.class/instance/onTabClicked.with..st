event handling
onTabClicked: anEvent with: aBrick 
	"A tab has been clicked."
	
	self selectedIndex: (self tabs indexOf: aBrick)
operations
tabClicked: evt with: aMorph 
	"A tab has been clicked."
	
	self selectedIndex: (self tabs indexOf: aMorph)
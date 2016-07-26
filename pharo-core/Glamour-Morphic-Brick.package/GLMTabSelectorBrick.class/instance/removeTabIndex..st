adding/removing
removeTabIndex: anInteger
	"Remove the tab at the given index and
	adjust any selected (next or first if was last)."
	
	self tabs removeAt: anInteger.
	self selectedIndex: 0.
	self tabs ifNotEmpty: [
		self selectedIndex: self selectedIndex - 1 \\ self tabs size + 1]
		
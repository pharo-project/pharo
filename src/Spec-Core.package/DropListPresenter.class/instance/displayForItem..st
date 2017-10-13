api
displayForItem: anItem
	"The order of the arguments may look weird, but then it seems more natural while using the widget"
	
	^ self displayBlock cull: anItem model cull: anItem
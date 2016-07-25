geometry
fitScrollTargetWidth
	"if the scroller extent is different that the textArea 
	extent, then, if the textArea is wrapped, then resize 
	the textArea width accordingly
	If the textArea is not wrapped, then do nothing
	"
	self scroller width = self textArea width
		ifFalse: [ self textArea width: self scrollBounds width ]
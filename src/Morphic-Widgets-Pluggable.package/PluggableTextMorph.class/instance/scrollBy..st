private
scrollBy: delta
	"Move the contents in the direction delta."
	
	super scrollBy: delta.
	
	self triggerEvent: #hScroll with: self scrollValue x.
	self triggerEvent: #vScroll with: self scrollValue y
	
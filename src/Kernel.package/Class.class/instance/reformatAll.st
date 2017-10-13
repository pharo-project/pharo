compiling
reformatAll 
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"

	super reformatAll.		"me..."
	self theMetaClass reformatAll.	"...and my metaclass"
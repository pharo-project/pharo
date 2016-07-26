construction
addMenuItem: anItem
	"Like addMorphBack: but returns the argument. The return value is used by the
	#addToggle:target:selector: and #addTarget:selector: families of messages."

	self addMorphBack: anItem.
	^ anItem

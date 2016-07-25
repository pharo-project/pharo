*SUnit-UITesting
itemWithWording: wording
	"If any of the receiver's items or submenu items have the given wording (case-blind comparison done), then return it, else return nil."
	
	self items do:[:anItem | | found |
		found := anItem itemWithWording: wording.
		found ifNotNil:[^found]].
	^ nil
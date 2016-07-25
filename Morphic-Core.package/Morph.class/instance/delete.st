submorphs-add/remove
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."
	
	self removeHalo.
	self activeHand 
		releaseKeyboardFocus: self;
		releaseMouseFocus: self.
	owner ifNotNil:[ 
		self privateDelete.
		self announceDeleted.
	].
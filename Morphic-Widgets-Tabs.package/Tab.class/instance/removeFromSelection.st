private
removeFromSelection

	self triggerEvent: #tabRemovedFromSelection with: self.
	
	"No need to change here since the toolbar I belong to will do it"
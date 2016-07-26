backward compatibility
removeEmptyCategories
	| oldCategories |
	
	oldCategories := self protocolOrganizer allProtocolsNames copy.
	self protocolOrganizer removeEmptyProtocols.
	self notifyOfChangedCategoriesFrom: oldCategories to: self protocolOrganizer allProtocolsNames.
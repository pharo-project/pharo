accessing
removeProtocol: aProtocol
	| oldCategories |
	
	oldCategories := self protocolOrganizer allProtocolsNames copy.
	self protocolOrganizer removeProtocol: aProtocol.
	self notifyOfRemovedCategory: aProtocol name.
	self 
		notifyOfChangedCategoriesFrom: oldCategories 
		to: self protocolOrganizer allProtocolsNames.
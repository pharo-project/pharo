backward compatibility
removeEmptyCategories
	| oldCategories removedProtocols |
	
	oldCategories := self protocolOrganizer allProtocolsNames copy.
	
	removedProtocols := self protocolOrganizer removeEmptyProtocols.	
	removedProtocols do: [ :each |  self notifyOfRemovedCategory: each name ].
	
	self notifyOfChangedCategoriesFrom: oldCategories to: self protocolOrganizer allProtocolsNames.
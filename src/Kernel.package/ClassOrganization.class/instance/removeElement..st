backward compatibility
removeElement: aSymbol
	| oldProtocol |
	
	oldProtocol := self categoryOfElement: aSymbol.
	self protocolOrganizer removeMethod: aSymbol.
	self notifyOfChangedSelector: aSymbol from: oldProtocol to: (self categoryOfElement: aSymbol).
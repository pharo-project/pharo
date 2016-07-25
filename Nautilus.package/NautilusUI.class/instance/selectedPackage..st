accessing
selectedPackage: aPackage
	" Force package selection, not used by the lists "
	
	self okToChange ifFalse: [ ^ self ].

	acceptor := ClassDefinitionAcceptor model: self.
			
	self resetPackageGroupsListSelection.
	self packageWidget selectPackage: aPackage.
		
	aPackage 
		ifNotNil: [ 
			self basicSelectPackage: aPackage ]
		ifNil: [
			self selectedPackageWithoutChangingSelection: nil.
			self listClearSelection ].	
		
	
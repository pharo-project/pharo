accessing
selectedGroup: aGroup
	" Force package selection, not used by the lists "

	self okToChange ifFalse: [ ^ self ].
			
	acceptor := ClassDefinitionAcceptor model: self.
			
	self resetPackageGroupsListSelection.
	self packageWidget selectGroup: aGroup.
		
	aGroup 
		ifNotNil: [ 
			self basicSelectPackage: aGroup ]
		ifNil: [
			self selectedPackageWithoutChangingSelection: nil. 
			self listClearSelection ].	
		

	
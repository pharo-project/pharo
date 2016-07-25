private
unselectClass
	acceptor := ClassDefinitionAcceptor model: self.
	self resetClassesListSelection.
	self model
		package: self selectedPackage.
	self updateOnClassSelection
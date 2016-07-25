actions
addSlice
	(MCSliceMaker openFor: self window)
		ifNotNil: [:sliceInfo | workingCopy := sliceInfo makeSlice.
			workingCopy ifNotNil: [
				workingCopy repositoryGroup 
					addRepository: MCHttpRepository pharoRepository;
					addRepository: MCHttpRepository pharoInboxRepository.
				workingCopyWrapper := MCDependentsWrapper with: workingCopy model: self.
					self repositorySelection: 0.]].
	self workingCopyListChanged; changed: #workingCopySelection; repositoryListChanged.
	self changedButtons.

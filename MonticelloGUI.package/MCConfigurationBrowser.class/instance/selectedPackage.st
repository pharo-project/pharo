dependencies
selectedPackage
	^ self selectedDependency ifNotNil: [:dep | dep package]
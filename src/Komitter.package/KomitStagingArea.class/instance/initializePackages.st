accessing
initializePackages 

	packages := MCWorkingCopy allManagers
		select: [ :workingCopy | workingCopy needsSaving and: (filterBlock cull: workingCopy) ]
		thenCollect: [ :workingCopy | 
			(KomitPackage package: workingCopy package)
				yourself ].
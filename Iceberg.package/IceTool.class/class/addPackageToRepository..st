utilities
addPackageToRepository: repo
	| options descriptionBlock |
	
	descriptionBlock := 
		[:wc | wc modified 
			ifTrue: ['* ', wc packageName]
			ifFalse: [ wc packageName ]].
				
	options := (MCWorkingCopy allManagers difference: repo workingCopies).

	(self chooseFrom: options displaying: descriptionBlock) 
		ifNotNil: [ :workingCopy | 
			repo 
				savePackage: workingCopy packageName 
				withMessage: ('Importing package ', workingCopy packageName) ]

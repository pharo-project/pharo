actions
removeRequiredPackage

	| allRequiredPackages |
	workingCopy ifNil: [^ self].
	allRequiredPackages := workingCopy requiredPackages.
	allRequiredPackages ifEmpty: [UIManager default inform: 'This package has no requirements'. ^ self].
	(self pickWorkingCopySatisfying: [:wc | allRequiredPackages includes: wc package]) 
		ifNotNil:
			[:required |
			workingCopy removeRequiredPackage: required package.
			self workingCopyListChanged]
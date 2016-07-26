slice-making
makeSlice
	| workingCopy package |
	self canMakePackage ifFalse: [ ^ nil ].
	package := RPackageOrganizer default registerPackageNamed: self mcPackageName.
	workingCopy := package mcWorkingCopy.
	self includedPackages
		do: [:required | workingCopy requirePackage: required package].
	^ workingCopy
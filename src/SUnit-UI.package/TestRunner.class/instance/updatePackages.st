updating
updatePackages
	packages := self findPackages.
	packageIndex := 0.
	packagesSelected := packagesSelected isNil
		ifTrue: [ Set new ]
		ifFalse: [
			packagesSelected
				select: [ :each | packages includes: each ] ].
	self changed: #packageList; changed: #packageSelected.
running
checkPackage: aPackage
	self renrakuRule
		check: aPackage
		forCritiquesDo: [ ^ result addPackage: aPackage ]
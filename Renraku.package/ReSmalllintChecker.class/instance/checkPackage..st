private
checkPackage: aPackage

	(environment includesPackage: aPackage) ifTrue: [
		self getCritiquesAbout: aPackage by: packageRules ]
private registration
checkPackageExistsOrRegister: packageName
	(self packages 
		anySatisfy: [ :each | self category: packageName matches: each packageName ])
		ifFalse: [ (RPackage named: packageName capitalized) register ] 
private
checkPackage: aRPackage
	(environment includesPackage: aRPackage) 
		ifTrue: [rule checkPackage: aRPackage]
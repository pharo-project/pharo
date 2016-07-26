accessing
packageRules

	^ self allRules select: [ :rule | rule class checksPackage ]
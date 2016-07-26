method
moveMethod: aMethod inPackage: aPackage

	aMethod methodClass organization
				classify: aMethod selector
				under: '*', aPackage name
				suppressIfDefault: true.
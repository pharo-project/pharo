image package loading
setPackageOfClass: rgClass using: packageKeys
	"Set the package to aRGBehaviorDefinition"

	(self findPackageOfClass: rgClass using: packageKeys) 
		ifNotNil:[ :pck| pck addClass: rgClass ]
	
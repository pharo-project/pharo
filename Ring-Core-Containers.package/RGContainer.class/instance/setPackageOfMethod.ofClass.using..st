image package loading
setPackageOfMethod: rgMethod ofClass: rgClass using: packageKeys
	"Set the package to aRGMethodDefinition and its value isExtension"

	(self findPackageOfMethod: rgMethod using: packageKeys) 
		ifNil:[ rgClass package ifNotNil:[ :pck| 
				pck addMethod: rgMethod.
				rgMethod isExtension: false ] ]
		ifNotNil:[ :pck| 
			pck addMethod: rgMethod.
			rgMethod isExtension: (rgMethod package ~= rgClass package) ]
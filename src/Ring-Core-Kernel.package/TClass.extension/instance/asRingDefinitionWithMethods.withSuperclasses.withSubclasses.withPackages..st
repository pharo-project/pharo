*Ring-Core-Kernel
asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackages: packsBoolean
	"Retrieves a ring class/trait based on the receiver.
	The data loaded in the class/trait (active methods, superclasses, subclasses and packages) is requested by the users.
	As it may need to traverse hierarchies for retrieving super and subclasses a ring slice is created as the container for every class, method and package.
	To retrieve the slice:  aRGClass environment "
	| rgClass rgSlice  rgPackageKeys |
	
	rgSlice := RGSlice named: #fromImage.
	packsBoolean ifTrue: [ 
		rgPackageKeys := rgSlice loadPackagesFromImage ].
	rgClass := self asRingDefinitionWithMethods: methodsBoolean withSuperclasses: supersBoolean withSubclasses: subsBoolean withPackageKeys: rgPackageKeys in: rgSlice.
	rgSlice cleanEmptyPackages.
	rgSlice loadTraitUsers.
		
	^ rgClass
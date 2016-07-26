*Manifest-Core
manifestReadOn: aArray

	^ (self named:( aArray at:1))
		baseClass: (RGClassDefinition named: (aArray at:2))
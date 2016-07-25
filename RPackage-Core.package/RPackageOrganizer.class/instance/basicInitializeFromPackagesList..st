initialization-data
basicInitializeFromPackagesList: aPackagesList

	aPackagesList
		do: [ :packageName | 
		  | package |
		  package := (RPackage named: packageName).
		  packages at: packageName asSymbol put: package. ]
		displayingProgress: 'Importing monticello packages'.
		
	Smalltalk allClassesAndTraits 
		do: [ :eachBehavior | self initializeFor: eachBehavior ]
		displayingProgress: 'Importing behaviors'.

	Smalltalk allClassesAndTraits 
		do: [ :eachBehavior | 
			self initializeMethodsFor: eachBehavior.
			self initializeMethodsFor: eachBehavior theMetaClass ]
		displayingProgress: 'Importing methods'.

	Smalltalk allClassesAndTraits 
		do: [ :eachBehavior |
			eachBehavior organization extensionProtocols do: [ :eachProtocol | 
				self 
					initializeExtensionsFor: eachBehavior 
					protocol: eachProtocol ].
			eachBehavior theMetaClass organization extensionProtocols do: [ :eachProtocol | 
				self 
					initializeExtensionsFor: eachBehavior theMetaClass 
					protocol: eachProtocol ] ]
		displayingProgress: 'Importing extensions'.
accessing
dependenciesSize

	^ self dependencies 
			inject: 0
			into: [ :sum :dependency |
					sum + dependency numberOfDependencies
				]
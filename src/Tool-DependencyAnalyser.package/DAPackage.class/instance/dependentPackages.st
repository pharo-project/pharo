dependencies
dependentPackages
	^ self dependencies 
			inject: Set new 
			into: [ :dependents :each | dependents add: each target; yourself ].
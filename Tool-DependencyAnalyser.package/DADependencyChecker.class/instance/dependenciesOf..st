accessing
dependenciesOf: aPackageName
	^ report 
		dependenciesOf: aPackageName asSymbol
		ifAbsentPut: [ self computeDependenciesOf: aPackageName ]
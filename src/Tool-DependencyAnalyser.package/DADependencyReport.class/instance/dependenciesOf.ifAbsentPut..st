accessing
dependenciesOf: aPackageName ifAbsentPut: aBlock
	^ dependencies 
		at: aPackageName
		put: aBlock value
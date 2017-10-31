accessing
externalDependendiesOf: packagesCollection

	| dependencies | 
	
	dependencies := packagesCollection flatCollect: [ :aPackage | self dependenciesReport dependenciesOf: aPackage ] as: Set.
	
	dependencies := dependencies copyWithoutAll: packagesCollection.
	
	^ dependencies sorted
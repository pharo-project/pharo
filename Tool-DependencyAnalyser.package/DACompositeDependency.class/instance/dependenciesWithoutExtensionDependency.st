testing
dependenciesWithoutExtensionDependency
	^ dependencies reject: [ :dependency | dependency isExtensionDependency ].
accessing
children
	^ (self packageDependencies"GroupedByAssembly" sort: [ :a :b | a packageName < b packageName])
		copyWith: DAPotentialDependenciesNode new
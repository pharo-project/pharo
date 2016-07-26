private
dependencyChecker
	^ (DependencyChecker ifNil: [ DADependencyChecker ]) new
private
transitiveDependenciesOf: aPackageName visited: alreadyVisitedPackages
	| deps |
	deps := Set new.
	alreadyVisitedPackages add: aPackageName.
	deps 
		addAll: (dependencies at: aPackageName).
	deps
		reject: [ :packageName | alreadyVisitedPackages includes: packageName ]
		thenDo: [ :packageName | deps addAll: (self transitiveDependenciesOf: packageName visited: alreadyVisitedPackages) ].
	^ deps
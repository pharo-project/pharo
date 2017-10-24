publishing
htmlForIgnoredDependenciesOf: aPackageName
	^ String streamContents: [ :str |
		(RPackage organizer packageNamed: aPackageName) ignoredDependencies 
			do: [ :dependency | str << (self warningLabel: dependency) ]
			separatedBy: [ str space ] ]
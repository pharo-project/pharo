dependencies
compositeDependencyTo: aPackage
	^ self dependencies detect: [ :dependency | dependency target = aPackage ] ifNone: [ nil ]
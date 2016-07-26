private
computeTransitiveDependenciesOf: aPackageName visitedPackages: visited
	(visited includes: aPackageName) ifTrue: [ ^ self ].
	self computeDependenciesOf: aPackageName visitedPackages: visited.
	(self dependenciesOf: aPackageName) 
		do: [ :each | self computeTransitiveDependenciesOf: each visitedPackages: visited ]
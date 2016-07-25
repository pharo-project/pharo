private
createClassListFor: aPackageGroup

	aPackageGroup ifNil: [ ^ {}].
	^ (SortAlphabeticallyClassList buildListFor: aPackageGroup asNautilusSelection in: self model browsedEnvironment) sortedElements
private
isFilePartOfAPackage: aFileReferenceRelativeToRepositoryDirectory
	| packageSuffix segments |
	packageSuffix := '.package'.
	segments := aFileReferenceRelativeToRepositoryDirectory path segments.
	^ self subdirectory isEmptyOrNil
		ifTrue: [ segments first endsWith: packageSuffix ]
		ifFalse: [  (segments first = self subdirectory) and: [ segments second endsWith: packageSuffix ] ]
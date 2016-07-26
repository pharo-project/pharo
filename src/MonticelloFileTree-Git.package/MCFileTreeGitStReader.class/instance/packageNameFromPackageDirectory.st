private
packageNameFromPackageDirectory
	| filename |
	filename := self fileUtils directoryName: packageDirectory asFileReference.
	^ filename copyFrom: 1 to: (filename lastIndexOf: $.) - 1
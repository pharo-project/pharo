private
basicLoadWithNameLike: aString
	| loader |
	
	self checkForModificationsIfCancel: [ ^ self] ifMerge: [ ^ self mergeVersions ].
	
	loader := versions size > 1
		ifTrue: [MCMultiPackageLoader new]
		ifFalse: [MCPackageLoader new].
		
	versions do: [:ea |
		ea canOptimizeLoading
			ifTrue: [ea patch applyTo: loader]
			ifFalse: [loader updatePackage: ea package withSnapshot: ea snapshot]].
		
	loader loadWithNameLike: aString.
	versions do: [:ea | ea workingCopy loaded: ea]
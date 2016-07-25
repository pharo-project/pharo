api-configuration
modifiedPackageSpecs: versionStringOrSymbol packageSpecsDo: aBlock
	| versionSpec |
	versionSpec := (self project version: versionStringOrSymbol) spec.
	versionSpec
		projectDo: [ :ignored |  ]
		packageDo: [ :packageSpec | 
			| wc |
			wc := packageSpec workingCopy.
			wc ~~ nil
				ifTrue: [ 
					wc modified
						ifTrue: [ aBlock value: versionSpec value: packageSpec value: wc ] ] ]
		groupDo: [ :ignored |  ]
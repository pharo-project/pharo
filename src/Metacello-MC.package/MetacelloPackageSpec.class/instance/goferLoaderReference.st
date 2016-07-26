accessing
goferLoaderReference

	^file == nil 
		ifTrue: [ GoferPackageReference name: self name ]
		ifFalse: [ 
			"does Monticello-style #versionInfoFromVersionNamed: matching"
			MetacelloGoferPackage name: self name packageFilename: self file ]
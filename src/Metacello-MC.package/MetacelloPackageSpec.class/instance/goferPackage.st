accessing
goferPackage

	goferPackage == nil 
		ifTrue: [ goferPackage := MetacelloGoferPackage name: self name packageFilename: self file ].
	^goferPackage
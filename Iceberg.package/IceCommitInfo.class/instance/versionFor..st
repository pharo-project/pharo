as yet unclassified
versionFor: anIceSavedPackage 
	^ (self versionDescriptorFor: anIceSavedPackage)
		ifNotNil: [ :packageVersion |
	 		self repository repository versionFromFileNamed: packageVersion 
		]
		ifNil: [ self lookupVersionFor: anIceSavedPackage ]

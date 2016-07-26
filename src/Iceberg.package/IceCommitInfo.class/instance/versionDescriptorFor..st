as yet unclassified
versionDescriptorFor: anIceSavedPackage 
	^ self allVersionDescriptors 
		detect: [ :each | each packageName = anIceSavedPackage packageName ]
		ifNone: nil

Extension { #name : 'TheManifestBuilder' }

{ #category : '*Deprecated12' }
TheManifestBuilder class >> ofPackageNamed: aPackageName [

	self deprecated: 'Call #ofPackage: instead because the only senders of this method is in NewTools and has the package directly.'.
	^ self ofPackage: (self packageOrganizer packageNamed: aPackageName)
]

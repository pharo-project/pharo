package
addPackagesAsGroup: aCollection
	| group |
	
	aCollection isEmptyOrNil ifTrue: [ ^ nil ].
	
	group := super addPackagesAsGroup: aCollection.
	self updatePackageView.
	^ group
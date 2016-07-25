visitor
visitPackage: aMTPackage 
	self commitStrategy shouldCommitDirtyPackages ifFalse: [ ^ self ].
	
	aMTPackage isDirty ifFalse: [ ^ self ].
	(self packageVisited: aMTPackage) ifTrue: [ ^ self ].
	packages add: aMTPackage
actions
createRepository
	| backend locationToUse |
	
	self validate.

	locationToUse := self locationToUse.
	backend := self storageClass 
		newRepositoryAt: locationToUse
		subdirectory: self subdirectory.

	(locationToUse exists not or: [ locationToUse hasChildren not ])
		ifTrue: [ 
			backend 
				cloneRepositoryFrom: self remoteToUse 
				branch: self branchName ].

	^ IceRepository backend: backend
accessing
resultClass

	renrakuRule class checksNode    ifTrue: [ ^ RBSelectorEnvironment ].
	renrakuRule class checksMethod  ifTrue: [ ^ RBSelectorEnvironment ].
	renrakuRule class checksClass   ifTrue: [ ^ RBClassEnvironment ].
	renrakuRule class checksPackage ifTrue: [ ^ RBPackageEnvironment ].
	^ RBMultiEnvironment
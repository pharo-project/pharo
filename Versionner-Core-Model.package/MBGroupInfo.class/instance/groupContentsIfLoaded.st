accessing
groupContentsIfLoaded

	| version loadedPackageNames groupPackageNames |
	groupContents ~~ nil ifTrue: [ ^groupContents ].
	groupContents := #().
	self configurationInfo ifNil: [ ^ groupContents ].
	(version := self configurationInfo currentVersion) ifNil: [ ^groupContents ].
	loadedPackageNames := version defaultPackageNamesToLoad: #().
	loadedPackageNames remove: 'default' ifAbsent: [].
	groupPackageNames := (version packagesForSpecNamed: self name) collect: [:spc | spc name ].
	groupPackageNames do: [:nm | (loadedPackageNames includes: nm) ifFalse: [ ^groupContents ] ].
	^groupContents := self spec includes
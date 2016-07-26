utils
extractPackageSignatureFrom: aWorkingCopy
	| ref |
	(aWorkingCopy isNil or: [ aWorkingCopy ancestors isEmpty ])
		ifTrue: [ ^ '' ].
	ref := GoferVersionReference name: aWorkingCopy ancestors first name.
	^ ref author , '.' , ref versionNumber asString
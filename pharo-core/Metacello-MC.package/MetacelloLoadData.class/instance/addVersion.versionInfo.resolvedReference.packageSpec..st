accessing
addVersion: version versionInfo: versionInfo resolvedReference: resolvedReference packageSpec: packageSpec
	| vis |
	"check for duplicates and use the one that is being added"
	(vis := self packageNameMap at: packageSpec name ifAbsent: [  ]) ~~ nil
		ifTrue: [ 
			"remove old references"
			vis
				do: [ :vi | 
					self dataMap removeKey: vi name.
					self versionInfoMap removeKey: vi name ] ].
	self dataMap
		at: version info name
		put:
			{version.
			resolvedReference.
			packageSpec}.
	self versionInfoMap at: versionInfo name put: {versionInfo}.
	self packageNameMap at: packageSpec name put: {versionInfo}
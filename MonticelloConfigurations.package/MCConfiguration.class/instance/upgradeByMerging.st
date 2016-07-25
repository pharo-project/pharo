actions
upgradeByMerging
	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isFulfilledByAncestors not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^(versions noneSatisfy: [:ver | self mustMerge: ver])
		ifTrue: [self loadVersions: versions]
		ifFalse: [self mergeVersionsSilently: versions].

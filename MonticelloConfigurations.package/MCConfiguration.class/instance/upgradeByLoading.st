actions
upgradeByLoading
	"this differs from #load only in that newer versions in the image are not downgraded"

	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isFulfilledByAncestors not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^self loadVersions: versions

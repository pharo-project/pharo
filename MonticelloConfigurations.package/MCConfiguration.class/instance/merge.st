actions
merge
	"merge in all the versions in this configuration"

	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isFulfilledByAncestors not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^self mergeVersions: versions
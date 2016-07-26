actions
load
	"install all the versions in this configuration, even if this means to downgrade a package"

	| versions |
	versions := OrderedCollection new.

	self depsSatisfying: [:dep | dep isCurrent not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	^self loadVersions: versions
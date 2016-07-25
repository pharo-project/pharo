actions
migrate
	"copy all packageversions in this cofiguration to a repository"
	| versions |
	versions := OrderedCollection new.

	configuration depsSatisfying: [:dep | dep isFulfilledByAncestors not]
		versionDo: [:ver | versions add: ver]
		displayingProgress: 'finding packages'.

	self pickRepository ifNotNil: [:aRepository |
		versions do: [:eachVersion |
			Transcript cr; show: '',aRepository,' storeVersion: ', eachVersion.
			aRepository storeVersion: eachVersion
		]
	]

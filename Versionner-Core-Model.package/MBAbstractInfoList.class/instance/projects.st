accessing-computed
projects
	"Return the list of dependent packages for the last baseline"

	| projectSpecs |
	projects
		ifNotNil: [ ^ projects ].
	projects := OrderedCollection new.
	self versionForInfoList ifNil: [ ^ projects ].
	projectSpecs := self versionForInfoList projects.
	projectSpecs := projectSpecs asSortedCollection: [ :a :b | a name <= b name ].
	projects
		addAll:
			(projectSpecs
				collect: [ :prjSpec | 
					(MBProjectInfo named: prjSpec name packageName: prjSpec projectPackage name spec: prjSpec)
						configurationInfo: self configurationInfo;
						yourself ]).
	^ projects
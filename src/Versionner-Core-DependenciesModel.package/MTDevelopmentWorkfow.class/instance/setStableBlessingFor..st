private
setStableBlessingFor: versionName
	(MetacelloToolBox configurationNamed: project name)
		modifyVersionMethodForVersion: versionName
			versionSpecsDo: [ :attr :versionSpec | 
					attr == #common
						ifTrue: [ versionSpec blessing: #stable ].
					true ];
		commitMethod
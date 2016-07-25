zodiacRepo := MCHttpRepository
	location: 'http://mc.stfx.eu/Zodiac'
	user: ''
	password: ''.

pharoRepo := MCHttpRepository
	location: 'http://smalltalkhub.com/mc/Pharo/Pharo60/main'
	user: ''
	password: ''.

MCWorkingCopy allManagers
	select: [ :m | m packageName beginsWith: 'Zodiac' ]
	thenDo: [ :m | m repositoryGroup addRepository: zodiacRepo ].

[MCWorkingCopy allManagers
		do: [ :manager | | version |
			FileStream stdout nextPutAll: 'Exporting', manager packageName; cr.
			manager repositoryGroup addRepository: pharoRepo.
			version := manager repositoryGroup versionWithInfo: manager ancestors first.
			MCFileTreeGitWriter
				fileOut: version
				on: (MCFileTreeRepository new directory: 'pharo-core' asFileReference ensureCreateDirectory)
				] ] on: Warning do: #resume.
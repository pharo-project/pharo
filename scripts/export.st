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

repo := (MCFileTreeGitRemoteRepository basicFromUrl: 'gitfiletree://github.com/guillep/pharo-core' asZnUrl)
	setNoMetadata;
	yourself.
	
[MCWorkingCopy allManagers
		do: [ :manager | | version |
			manager repositoryGroup addRepository: pharoRepo.
			version := manager repositoryGroup versionWithInfo: manager ancestors first.
			MCFileTreeGitWriter fileOut: version on: repo ] ] on: Warning do: #resume.
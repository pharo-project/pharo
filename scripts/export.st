| zodiacRepo pharoRepo mosseAlgosRepo versions mooseAlgosRepo icebergTarget libgitTarget cloneRepo iceRepo libgitRepo | 

zodiacRepo := MCHttpRepository
	location: 'http://mc.stfx.eu/Zodiac'
	user: ''
	password: ''.

pharoRepo := MCHttpRepository
	location: 'http://smalltalkhub.com/mc/Pharo/Pharo60/main'
	user: ''
	password: ''.
	
mooseAlgosRepo :=  MCHttpRepository
	location: 'http://smalltalkhub.com/mc/Moose/MooseAlgos/main/'
	user: ''
	password: ''.

MCWorkingCopy allManagers
	select: [ :m | m packageName beginsWith: 'Zodiac' ]
	thenDo: [ :m | m repositoryGroup addRepository: zodiacRepo ].

MCWorkingCopy allManagers
	select: [ :m | m packageName beginsWith: 'Moose' ]
	thenDo: [ :m | m repositoryGroup addRepository: mooseAlgosRepo ].
	
icebergTarget := ('pharo-local' asFileReference / 'Iceberg') ensureCreateDirectory.
libgitTarget := ('pharo-local' asFileReference / 'LibGit') ensureCreateDirectory.

cloneRepo := IceRepositoryCreator new 
	remote: (IceRemote url: 'git@github.com:pharo-vcs/Iceberg.git');
	location: icebergTarget;
	createRepository.	
cloneRepo backend checkoutBranch: 'v0.4'.	

cloneRepo := IceRepositoryCreator new 
	remote: (IceRemote url: 'https://github.com/pharo-vcs/libgit2-pharo-bindings.git');
	location: libgitTarget;
	createRepository.	
cloneRepo backend checkoutBranch: 'development'.


iceRepo := MCFileTreeRepository new
	directory: icebergTarget ;
	yourself.

libgitRepo := MCFileTreeRepository new
	directory: libgitTarget ;
	yourself.
	
MCWorkingCopy allManagers
	select: [ :m | m packageName includesSubstring: 'Iceberg' ]
	thenDo: [ :m | m repositoryGroup addRepository: iceRepo ].
	
MCWorkingCopy allManagers
	select: [ :m | #('BaselineOfLibGit' 'LibGit-Core') includes: m packageName ]
	thenDo: [ :m | m repositoryGroup addRepository: libgitRepo ].

versions := ScriptLoader new perform: (#script, SystemVersion current highestUpdate asString) asSymbol.

"add ScriptLoader package itself"
versions add: (#ScriptLoader60 asPackage mcWorkingCopy ancestry ancestors first name, '.mcz').

versions := versions sorted: [ :v1 :v2 | v1 < v2 ].
[versions do: [ :versionName | | packageName manager |
	packageName := ($- join: (versionName substrings: '-') allButLast) copyUpTo: $..
	manager := MCWorkingCopy hasPackageNamed: packageName.
	manager == false
		ifTrue: [ FileStream stdout nextPutAll: 'Package with no working copy: ', packageName; cr. ]
		ifFalse: [
			FileStream stdout nextPutAll: 'Exporting ', packageName; cr.
			manager repositoryGroup addRepository: pharoRepo.
			version := manager repositoryGroup versionWithInfo: manager ancestors first.
			('src' asFileReference / packageName , '.package') ensureDeleteAll.
			IceMetadatalessFileTreeWriter
				fileOut: version
				on: (MCFileTreeRepository new directory: 'src' asFileReference ensureCreateDirectory) ] ] ] on: Warning do: #resume.

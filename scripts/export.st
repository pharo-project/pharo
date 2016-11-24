Gofer it
	smalltalkhubUser: 'Pharo' project: 'Bootstrap';
	package: 'PharoBootstrap-Initialization';
	load.

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

bootInitPackageManager := (MCWorkingCopy hasPackageNamed: 'PharoBootstrap-Initialization').
MCFileTreeGitWriter
	fileOut: (bootInitPackageManager repositoryGroup versionWithInfo: bootInitPackageManager ancestors first)
	on: (MCFileTreeRepository new directory: 'src' asFileReference ensureCreateDirectory).

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
			MCFileTreeGitWriter
				fileOut: version
				on: (MCFileTreeRepository new directory: 'src' asFileReference ensureCreateDirectory) ] ] ] on: Warning do: #resume.

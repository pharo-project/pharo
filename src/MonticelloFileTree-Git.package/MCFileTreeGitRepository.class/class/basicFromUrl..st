instance creation
basicFromUrl: aZnUrl
	"A gitfiletree url with a host is the target for a remote. All other parameters are optional.
	Parameters are:
		dir : the directory inside the repository where the target MC packages are.
		branch : the git branch to fetch.
		protocol: the user name part to add to the ssh Url, default to git, but can also be https (which implies read only access).
		readOnly : is the repository read only? If present, reduce the history to a minimum (and change the GUI).
	Alternative url syntax:
		gitfiletree://github.com/dalehenrich/filetree:pharo5.0_dev/repository
	with:
		host : github.com
		project : dalehenrich/filetree
		branch : pharo5.0_dev
		dir : repository
"

	| repo |
	repo := self new.
	self assert: aZnUrl scheme = #gitfiletree.
	aZnUrl hasHost
		ifTrue: [ | protocol path |
			repo subdirectory: (self extractDir: aZnUrl).
			repo branch: (self extractBranch: aZnUrl).
			path := self extractPath: aZnUrl.
			protocol := aZnUrl queryAt: 'protocol' ifAbsent: [ 'git' ].
			repo readonly: (aZnUrl queryAt: 'readOnly' ifAbsent: [  ]) notNil.
			(aZnUrl hasUsername and: [ aZnUrl hasPassword ])
				ifTrue: [ protocol := 'https' ].
			protocol = 'git'
				ifTrue: [ repo remoteUrl: 'git@' , aZnUrl host , ':' , path ].
			protocol = 'https'
				ifTrue: [ | targetUrl |
					targetUrl := 'https://' asZnUrl.
					targetUrl host: aZnUrl host.
					targetUrl addPathSegments: ($/ split: path).
					targetUrl username: aZnUrl username.
					targetUrl password: aZnUrl password.
					repo remoteUrl: targetUrl printString ].
			repo
				gitCloneRepositoryAndCheckoutIn:
					(path
						ifNil: [ aZnUrl host ]
						ifNotNil: [ :n | n asFileReference basenameWithoutExtension ]).
			(repo isGitRepository: repo directory)
				ifFalse: [ MCFileTreeGitError new
						signal: 'This url is not a git repository' , aZnUrl printString ] ]
		ifFalse: [ repo directory: (self urlAsFileReference: aZnUrl) ].
	^ repo
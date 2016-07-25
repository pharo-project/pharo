instance creation
location: aGitUrl name: aName subdirectory: aDirectoryName branch: aBranch
	"Create this directly."

	| repo |
	repo := self new.
	repo
		subdirectory:
			(aDirectoryName isEmpty
				ifTrue: [ '' ]
				ifFalse: [ aDirectoryName ]).
	repo
		branch:
			(aBranch isEmpty
				ifTrue: [ nil ]
				ifFalse: [ aBranch ]).
	repo remoteUrl: aGitUrl.
	repo readonly: false.
	repo
		gitCloneRepositoryAndCheckoutIn: (aName ifEmpty: [ self extractFolderNameFrom: aGitUrl ]).
	^ repo

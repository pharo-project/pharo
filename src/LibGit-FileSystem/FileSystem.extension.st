Extension { #name : #FileSystem }

{ #category : #'*LibGit-FileSystem' }
FileSystem class >> onGitCommit: aCommit [
	^ self store: (LGitCommitStore on: aCommit)
]

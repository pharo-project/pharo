Class {
	#name : #IceGitRemoteBranch,
	#superclass : #IceRemoteBranch,
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #accessing }
IceGitRemoteBranch >> commit [
	
	repository handleLibgitError: [ | gitCommit gitId |
		gitId := (self repositoryHandle lookupRemoteBranch: self name) targetId.
		gitCommit := LGitCommit of: self repositoryHandle fromId: gitId.
		^ self repository commitFromGitCommit: gitCommit ]
]

{ #category : #'API-branches' }
IceGitRemoteBranch >> delete [
	
	repository handleLibgitError: [ | found |
		found := self repositoryHandle lookupRemoteBranch: self name ifAbsent: [ nil ].
		self repositoryHandle deleteBranch: found ]
]

{ #category : #filesystem }
IceGitRemoteBranch >> fileSystem [
	^ self commit fileSystem
]

{ #category : #accessing }
IceGitRemoteBranch >> gitRef [
	
	^ self name
]

{ #category : #accessing }
IceGitRemoteBranch >> name [

	^ self fullname withoutPrefix: self prefix
]

{ #category : #accessing }
IceGitRemoteBranch >> prefix [

	^ 'refs/remotes/'
]

{ #category : #accessing }
IceGitRemoteBranch >> remoteName [
	
	^ (self fullname substrings: '/') third
]

{ #category : #accessing }
IceGitRemoteBranch >> repositoryHandle [

	<repositoryHandleAccessor>
	^ repository repositoryHandle
]

{ #category : #accessing }
IceGitRemoteBranch >> shortname [

	^ self name withoutPrefix: self remoteName, '/'
]

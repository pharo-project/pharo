Extension { #name : #MCGitHubRepository }

{ #category : #'*Iceberg-Adapters' }
MCGitHubRepository >> commitId [
	"This kind of repo is associated to a specific commit that was downloaded, 
	sadly the only way to know it is to parse the directory name."
	| repoRoot |
	repoRoot := self repoPath isEmpty ifTrue: [ self directory ] ifFalse: [ self directory parent ].
	^ repoRoot basename copyAfterLast: $-.
]

{ #category : #'*Iceberg-Adapters' }
MCGitHubRepository class >> commitIdFor: aWorkingCopy [
	^ aWorkingCopy repositoryGroup repositories 
		detect: [ :repo | repo isKindOf: self ] 
		ifFound: #commitId
		ifNone: nil

]

{ #category : #'*Iceberg-Adapters' }
MCGitHubRepository >> commitIdFor: aMCWorkingCopy ifFound: aBlockClosure [ 
	(aMCWorkingCopy repositoryGroup includes: self)
		ifTrue: [ aBlockClosure value: self commitId ]
]

{ #category : #'*Iceberg-Adapters' }
MCGitHubRepository >> httpsUrl [
	^ 'https://github.com/', projectPath, '.git'
]

{ #category : #'*Iceberg-Adapters' }
MCGitHubRepository >> scpUrl [
	^ 'git@github.com:', projectPath, '.git'
]

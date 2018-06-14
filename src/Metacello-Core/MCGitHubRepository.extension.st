Extension { #name : #MCGitHubRepository }

{ #category : #'*Metacello-Core' }
MCGitHubRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createGithubRepository: aRepositorySpec
]

{ #category : #'*Metacello-Core' }
MCGitHubRepository class >> isAvailableFor: type [
	^ type = 'github'
]

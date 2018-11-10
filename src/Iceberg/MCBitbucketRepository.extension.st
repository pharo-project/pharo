Extension { #name : #MCBitbucketRepository }

{ #category : #'*Iceberg-Adapters' }
MCBitbucketRepository >> httpsUrl [
	^ 'https://bitbucket.org/', projectPath, '.git'
]

{ #category : #'*Iceberg-Adapters' }
MCBitbucketRepository >> scpUrl [
	^ 'git@bitbucket.org:', projectPath, '.git'
]

Extension { #name : #MCDirectoryRepository }

{ #category : #'*Metacello-Core' }
MCDirectoryRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createDirectoryRepository: aRepositorySpec
]

{ #category : #'*Metacello-Core' }
MCDirectoryRepository class >> isAvailableFor: type [
	^ self = MCDirectoryRepository and: [ type = 'directory' ]
]

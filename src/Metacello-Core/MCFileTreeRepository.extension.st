Extension { #name : #MCFileTreeRepository }

{ #category : #'*Metacello-Core' }
MCFileTreeRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createFiletreeRepository: aRepositorySpec
]

{ #category : #'*Metacello-Core' }
MCFileTreeRepository class >> isAvailableFor: type [
	^ self = MCFileTreeRepository and: [ type = 'filetree' ]
]

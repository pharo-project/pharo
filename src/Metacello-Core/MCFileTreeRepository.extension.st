Extension { #name : #MCFileTreeRepository }

{ #category : #'*Metacello-Core' }
MCFileTreeRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createFiletreeRepository: aRepositorySpec
]

{ #category : #'*Metacello-Core' }
MCFileTreeRepository class >> isAvailableFor: type [
	^ type = 'filetree'
]

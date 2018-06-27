Extension { #name : #MCFileTreeRepository }

{ #category : #'*Metacello-MC' }
MCFileTreeRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createFiletreeRepository: aRepositorySpec
]

{ #category : #'*Metacello-MC' }
MCFileTreeRepository class >> isAvailableFor: type [
	^ type = 'filetree'
]

Extension { #name : #MCDictionaryRepository }

{ #category : #'*Metacello-Core' }
MCDictionaryRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createDictionaryRepository: aRepositorySpec
]

{ #category : #'*Metacello-Core' }
MCDictionaryRepository class >> isAvailableFor: type [
	^ type = 'dictionary'
]

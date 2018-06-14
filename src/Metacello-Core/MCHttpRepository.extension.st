Extension { #name : #MCHttpRepository }

{ #category : #'*Metacello-Core' }
MCHttpRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createHttpRepository: aRepositorySpec
]

{ #category : #'*Metacello-Core' }
MCHttpRepository class >> isAvailableFor: type [
	^ self = MCHttpRepository and: [ type = 'http' ]
]

Extension { #name : #MCFtpRepository }

{ #category : #'*Metacello-Core' }
MCFtpRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createFtpRepository: aRepositorySpec
]

{ #category : #'*Metacello-Core' }
MCFtpRepository class >> isAvailableFor: type [
	^ type = 'ftp'
]

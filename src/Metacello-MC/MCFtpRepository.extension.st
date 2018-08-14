Extension { #name : #MCFtpRepository }

{ #category : #'*Metacello-MC' }
MCFtpRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createFtpRepository: aRepositorySpec
]

{ #category : #'*Metacello-MC' }
MCFtpRepository class >> isAvailableFor: type [
	^ type = 'ftp'
]

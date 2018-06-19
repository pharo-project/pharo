Extension { #name : #MCDirectoryRepository }

{ #category : #'*metacello-mc' }
MCDirectoryRepository >> asRepositorySpecFor: aMetacelloMCProject [
  ""

  ^ directory asRepositorySpecFor: aMetacelloMCProject
]

{ #category : #'*Metacello-MC' }
MCDirectoryRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createDirectoryRepository: aRepositorySpec
]

{ #category : #'*Metacello-MC' }
MCDirectoryRepository class >> isAvailableFor: type [
	^ type = 'directory'
]

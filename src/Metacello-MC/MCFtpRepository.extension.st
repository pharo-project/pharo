Extension { #name : 'MCFtpRepository' }

{ #category : '*Metacello-MC' }
MCFtpRepository class >> createRepositoryFromSpec: aRepositorySpec [

	| description headerSize index |
	description := aRepositorySpec description.
	headerSize := 'ftp://' size.
	index := description indexOf: $/ startingAt: headerSize + 1.

	^ self
		  host: (description copyFrom: headerSize + 1 to: index - 1)
		  directory: (description copyFrom: index + 1 to: description size)
		  user: aRepositorySpec username
		  password: aRepositorySpec password
]

{ #category : '*Metacello-MC' }
MCFtpRepository class >> isAvailableFor: type [
	^ type = 'ftp'
]

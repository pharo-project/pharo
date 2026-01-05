Extension { #name : 'MCDirectoryRepository' }

{ #category : '*Metacello-Core' }
MCDirectoryRepository >> asRepositorySpec [

	^ directory asRepositorySpec
]

{ #category : '*Metacello-Core' }
MCDirectoryRepository class >> createRepositoryFromSpec: aRepositorySpec [

	^ self new
		  directory: aRepositorySpec description asFileReference;
		  yourself
]

{ #category : '*Metacello-Core' }
MCDirectoryRepository class >> isAvailableFor: type [
	^ type = 'directory'
]

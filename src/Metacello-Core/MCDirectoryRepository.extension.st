Extension { #name : 'MCDirectoryRepository' }

{ #category : '*Metacello-Core' }
MCDirectoryRepository >> asRepositorySpec [

	^ (MetacelloRepositorySpec description: directory fullName)
		  type: 'directory';
		  yourself
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

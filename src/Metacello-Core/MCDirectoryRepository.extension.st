Extension { #name : 'MCDirectoryRepository' }

{ #category : '*Metacello-Core' }
MCDirectoryRepository >> asRepositorySpecFor: aMetacelloMCProject [
  ""

  ^ directory asRepositorySpecFor: aMetacelloMCProject
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

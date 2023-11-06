Extension { #name : 'MCDirectoryRepository' }

{ #category : '*Metacello-MC' }
MCDirectoryRepository >> asRepositorySpecFor: aMetacelloMCProject [
  ""

  ^ directory asRepositorySpecFor: aMetacelloMCProject
]

{ #category : '*Metacello-MC' }
MCDirectoryRepository class >> createRepositoryFromSpec: aRepositorySpec [

	^ self new
		  directory: aRepositorySpec description asFileReference;
		  yourself
]

{ #category : '*Metacello-MC' }
MCDirectoryRepository class >> isAvailableFor: type [
	^ type = 'directory'
]

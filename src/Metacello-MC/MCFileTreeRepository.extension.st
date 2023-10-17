Extension { #name : 'MCFileTreeRepository' }

{ #category : '*Metacello-MC' }
MCFileTreeRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [

	MCRepository findFiletreeAlternateFormat: aRepositorySpec ifFound: [ :repository | ^ repository createRepositoryFromSpec: aRepositorySpec on: aPlatform ].
	^ self new
		  directory: (aRepositorySpec description copyFrom: 'filetree://' size + 1 to: aRepositorySpec description size) asFileReference;
		  yourself
]

{ #category : '*Metacello-MC' }
MCFileTreeRepository class >> isAvailableFor: type [
	^ type = 'filetree'
]

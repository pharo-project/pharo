Extension { #name : 'FileReference' }

{ #category : '*Metacello-Core' }
FileReference >> asRepositorySpec [

	^ (MetacelloRepositorySpec description: self fullName)
		  type: 'directory';
		  yourself
]

Extension { #name : #FileSystemStore }

{ #category : #'*Zinc-Resource-Meta-Core' }
FileSystemStore >> mimeTypesAt: aPath [
	| mimeType |
	mimeType := ZnMimeType 
						forFilenameExtension: aPath extension 
						ifAbsent: [  ^ nil ].
	^ Array with: mimeType
]

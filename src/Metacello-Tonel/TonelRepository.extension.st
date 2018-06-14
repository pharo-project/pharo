Extension { #name : #TonelRepository }

{ #category : #'*Metacello-Tonel' }
TonelRepository class >> createRepositoryFromSpec: aRepositorySpec on: aPlatform [
	^ aPlatform createTonelRepository: aRepositorySpec
]

{ #category : #'*Metacello-Tonel' }
TonelRepository >> goferVersionFrom: aVersionReference [
	"Simillar hack than FileTree repositories."

	((self readableFileNames collect: [ :fileName | self fileDirectoryOn: fileName ])
		select: [ :packageDirectory | self fileUtils directoryExists: packageDirectory ])
		collect: [ :packageDirectory | 
			(self versionInfoForPackageDirectory: packageDirectory) name = aVersionReference name
				ifTrue: [ ^ self loadVersionFromFileNamed: (self fileUtils directoryName: packageDirectory) ] ].
	^ nil
]

{ #category : #'*Metacello-Tonel' }
TonelRepository class >> isAvailableFor: type [
	^ type = 'tonel'
]

{ #category : #'*Metacello-Tonel' }
TonelRepository class >> isFiletreeAlternateFormat: aRepositorySpec [
	| location propRef |
	
	location := (aRepositorySpec description allButFirst: ('filetree://' size)) asFileReference.
	propRef := location / '.properties'.
	propRef exists ifFalse: [ ^ false ].
	
	^ propRef readStreamDo:[ :stream | 
		((STONJSON fromStream: stream) at: #format ifAbsent: []) = #tonel ]
]

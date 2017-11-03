"
A MCDirectoryRepository that looks in subdirectories too.
"
Class {
	#name : #MCSubDirectoryRepository,
	#superclass : #MCDirectoryRepository,
	#category : #Monticello-Repositories
}

{ #category : #'user interface' }
MCSubDirectoryRepository class >> description [
	^ 'directory with subdirectories'
]

{ #category : #enumeration }
MCSubDirectoryRepository >> allDirectories [
	^directory allDirectories 
]

{ #category : #'user interface' }
MCSubDirectoryRepository >> description [
	^ (directory / '*') fullName

]

{ #category : #i/o }
MCSubDirectoryRepository >> findFullNameForReading: aBaseName [
	"Answer the latest version of aBaseName"
	^((directory glob: [ :each | each basename = aBaseName])
		sorted: [ :a :b | a modificationTime < b modificationTime ])
		first.
]

{ #category : #i/o }
MCSubDirectoryRepository >> findFullNameForWriting: aBaseName [
	| possible split prefix fpattern now |
	split := self splitNameVersionExtensionFor: aBaseName.
	fpattern := split first, '*'.
	possible := SortedCollection sortBlock: [ :a :b |
		a first = b first
			ifTrue: [ a second = b second
					ifFalse: [ a second < b second ]
					ifTrue: [ a third fullName size < b third fullName size ]]
			ifFalse: [ a first > b first ] ].
	now := Time totalSeconds.
	prefix := directory pathSegments size.
	self allDirectories do: [:dir | | dirScore fileScore parts |
		parts := dir pathSegments allButFirst: prefix.
		dirScore := (parts select: [ :part | fpattern match: part ]) size.
		fileScore := (dir entries collect: [ :ent |
			(ent isDirectory not and: [ fpattern match: ent name ])
				ifFalse: [ SmallInteger maxVal ]
				ifTrue: [ now - ent modificationTime asSeconds ]]).	"minimum age"
		fileScore := fileScore isEmpty ifTrue: [ SmallInteger maxVal  ]
			ifFalse: [ fileScore min ].
		possible add: { dirScore. fileScore. dir } ].
	
	^ (possible first third) / aBaseName
]

{ #category : #interface }
MCSubDirectoryRepository >> loadAllFileNames [
	"sorting {entry. dirName. name}"

	| sorted |
	sorted := SortedCollection sortBlock: [:a :b |
		a first modificationTime >= b first modificationTime ].
	self allDirectories
		do: [:dir | dir entries
				do: [:ent | ent isDirectory
						ifFalse: [sorted add: {ent. dir fullName. ent basename}]]].
	^ sorted
		collect: [:ea | ea third ]
]

{ #category : #i/o }
MCSubDirectoryRepository >> readStreamForFileNamed: aString do: aBlock [
	| file val |
	file := FileStream readOnlyFileNamed: (self findFullNameForReading: aString).
	val := aBlock value: file.
	file close.
	^ val
]

{ #category : #i/o }
MCSubDirectoryRepository >> splitNameVersionExtensionFor: aBaseName [

	| file version |
	file := aBaseName asFileReference.
	version := (file extensions at: file extensions size - 1) asNumber.
	^ { (file base copyUpTo: $.). version. file extension }.
	
	
	

]

{ #category : #i/o }
MCSubDirectoryRepository >> writeStreamForFileNamed: aString replace: aBoolean do: aBlock [ 
	| file |
	file := aBoolean
				ifTrue: [FileStream
						forceNewFileNamed: (self findFullNameForReading: aString)]
				ifFalse: [FileStream
						newFileNamed: (self findFullNameForWriting: aString)].
	aBlock value: file.
	file close
]

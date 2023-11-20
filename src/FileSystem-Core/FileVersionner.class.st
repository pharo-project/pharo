"
A class for managing file versioning based on the input basename and extension.
    
This class can handle both cases when the given file has numbers in its name and when the given file name should only extract version numbers from files without a number pattern.
    
## Attributes:
   
- basename (`String`): The base name of the file.
- extension (`String`): The file extension.
    
## Core methods

- nextVersion : Answer the next available version number for the file.
- lastFileFor:extension: Returns the latest version number if the file name is already numbered; otherwise, returns 0.
"
Class {
	#name : 'FileVersionner',
	#superclass : 'Object',
	#instVars : [
		'fileReference'
	],
	#category : 'FileSystem-Core-Public',
	#package : 'FileSystem-Core',
	#tag : 'Public'
}

{ #category : 'instance creation' }
FileVersionner class >> from: aFileReference [

	^ self new
		fileReference: aFileReference;
		yourself
]

{ #category : 'versions' }
FileVersionner >> extractVersionNumberFrom: filename basename: basename extension: extension [
	"Answer the version number written in filename considering
	that it consists of basename and extension.
	For example filename=basename.10.extension will return 10.
	filename=basename.extension without number will return 0
	It returns nil if given file does not satisfy this pattern"

	| tokenizedFilename |
	^ (tokenizedFilename := filename findTokens: '.') size = 2
		ifTrue: [ 0 ]
		ifFalse: [ 
			(tokenizedFilename
				collect: [ : e | NumberParser parse: e onError: [ nil ] ]
				thenReject: #isNil)
					ifEmpty: [ 0 ] 
					ifNotEmpty: [ : numbers | numbers last ] ].

]

{ #category : 'private' }
FileVersionner >> extractVersionNumberWithoutNumberPatternFrom: filename basename: basename extension: extension [ 

	| versionStart versionEnd |

	"ensure prefix basename."
	(filename beginsWith: basename) ifFalse: [^nil].
	(filename size > basename size and: [(filename at: basename size + 1) = $.])
		ifFalse: [ ^nil ].
	"ensure suffix .extension"
	(filename endsWith: extension) ifFalse: [^nil].
	(filename size > extension size and: [(filename at: filename size - extension size) = $.])
		 ifFalse: [ ^nil ].

	versionStart := basename size + 2.
	versionEnd := filename size - extension size - 1.
	versionStart to: versionEnd do: [:i |
		(filename at: i) isDigit ifFalse: [ ^nil ] ].
	versionStart > versionEnd ifTrue: [ ^0]. "means pattern baseline.extension"
	^(filename copyFrom: versionStart to: versionEnd) asNumber
]

{ #category : 'accessing' }
FileVersionner >> fileReference [

	^ fileReference
]

{ #category : 'accessing' }
FileVersionner >> fileReference: anObject [

	fileReference := anObject
]

{ #category : 'accessing' }
FileVersionner >> lastFileFor: baseFileName extension: extension [
	"Assumes a file is named using a version number encoded as '.' followed by digits
  preceding the file extension, e.g., games.22.ston
  Answer the file name with the largest number.
  If a version number is not found, raises an error"

	"FileSystem workingDirectory lastFileFor: 'games' extension: 'ston'"

	^ ((self versionNumberWithoutNumberPatternFor: baseFileName extension: extension)
		ifNil: [ self error: 'No file with number pattern' ]
		ifNotNil: [ : version | version = 0
			ifTrue: [ baseFileName, '.', extension ]
			ifFalse: [ baseFileName , '.' , version asString , '.' , extension ] ]) asFileReference
]

{ #category : 'accessing' }
FileVersionner >> nextVersion [
	"Assumes a file (or folder) name includes a version number encoded as '.' followed by digits
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, return just the file"

	| parent basename nameWithoutExtension extension max index |

	parent := self fileReference parent.
	extension := self fileReference extension.
	basename := self fileReference basename.
	nameWithoutExtension := self fileReference basenameWithoutExtension.
	
	"At this stage nameWithoutExtension may still include a version number.  Remove it if necessary"
	index := nameWithoutExtension size.
	[ index > 0 and: [ (nameWithoutExtension at: index) isDigit ] ] whileTrue:
		[ index := index - 1 ].
	((index between: 1 and: nameWithoutExtension size - 1) and: [ (nameWithoutExtension at: index) = $. ]) ifTrue:
		[ nameWithoutExtension := nameWithoutExtension copyFrom: 1 to: index-1 ].

	max := parent versionNumberFor: nameWithoutExtension extension: extension.
	^ parent / (nameWithoutExtension , '.' , (max + 1) asString , '.' , self fileReference extension)
]

{ #category : 'accessing' }
FileVersionner >> versionNumberFor: basename extension: extension [
	"Answer the latest (largest) version number for the specified file.
	0 = basename.extension exists, but nothing later.
	nil = no file exists"
	| maxVersion fileName |

	maxVersion := 0.
	"Handle the case of files with dots as part of its name"
	fileName := nil.
	(self fileReference childrenMatching: basename , '*', extension) 
		do: [ : fileRef | 
			(self extractVersionNumberFrom: fileRef basename basename: basename extension: extension)
				ifNotNil: [ : childVersion | 
					fileName := fileRef basenameWithoutExtension.
					maxVersion := maxVersion max: childVersion ] ].

	^ maxVersion = 0
		ifTrue: [ (((self fileReference / (basename , '.' , extension)) asFileReference exists) or: [ fileName isNotNil ])
			ifTrue: [ 0 ]
			ifFalse: [ nil ] ]
		ifFalse: [ maxVersion ]
]

{ #category : 'private' }
FileVersionner >> versionNumberWithoutNumberPatternFor: basename extension: extension [ 
	"Answer the latest (largest) version number for the specified file.
	0 = basename.extension exists, but nothing later.
	nil = no file exists"
	| maxVersion fileName |

	maxVersion := 0.
	"Handle the case of files with dots as part of its name"
	fileName := nil.
	(self fileReference childrenMatching: basename , '*', extension) 
		do: [ : fileRef | 
			(self extractVersionNumberWithoutNumberPatternFrom: fileRef basename basename: basename extension: extension)
				ifNotNil: [ : childVersion | 
					fileName := fileRef basenameWithoutExtension.
					maxVersion := maxVersion max: childVersion ] ].

	^ maxVersion = 0
		ifTrue: [ (((self fileReference / (basename, '.', extension)) asFileReference exists) or: [ fileName isNotNil ])
			ifTrue: [ 0 ]
			ifFalse: [ nil ] ]
		ifFalse: [ maxVersion ]
]

"
I'm an utility class to manage numbered file pattern e.g. 'games.12.ston' and I offers simple way to get the following version (e.g., 'games.13.ston') or the last version between a set of files. 

To work the user should explicitly set the pattern of the files. 
So far I define two.
- basename.number
- basename.number.extension

## Examples

```
vers := NewFileVersionner from:  '/Users/ducasse/Workspace/FirstCircle/Working/SF/sf/Comix/' asFileReference.
vers useBasenameNumberExtension.
vers fileReference: (vers lastFileFor: 'BD' extension: 'xml').
vers nextVersionFileReference 
```


## Core methods

- nextVersion : Answer the next available version number for the file.
- lastFileFor:extension: Returns the latest version number if the file name is already numbered; otherwise, returns 0.
"
Class {
	#name : 'NewFileVersionner',
	#superclass : 'Object',
	#instVars : [
		'fileReference',
		'pattern'
	],
	#category : 'FileSystem-Core-Public',
	#package : 'FileSystem-Core',
	#tag : 'Public'
}

{ #category : 'instance creation' }
NewFileVersionner class >> from: aFileReference [

 	^ self new fileReference: aFileReference ; yourself
]

{ #category : 'API' }
NewFileVersionner >> extractVersionOrZero [
	"Given a pattern basename.number.extension returns number.
	All the other case returns 0."
	
	| tokenizedFilename |
	pattern ifNil: [  Error signal: 'no pattern set!' ].
	
	tokenizedFilename := fileReference basename findTokens: '.'.
	^ tokenizedFilename size = pattern size
		ifFalse: [ 0 ]
		ifTrue: [ 
			| numberOrNil |
			numberOrNil := NumberParser parse: (tokenizedFilename at: (self indexOfNumber)) onError: [ nil ].
			numberOrNil ifNil: [ 0 ] ifNotNil: [ :n | n ]]

]

{ #category : 'accessing' }
NewFileVersionner >> fileReference [

	^ fileReference
]

{ #category : 'accessing' }
NewFileVersionner >> fileReference: aFileReference [

	fileReference := aFileReference
]

{ #category : 'pattern' }
NewFileVersionner >> indexOfNumber [

	^ pattern indexOf: #number
]

{ #category : 'API' }
NewFileVersionner >> lastFileFor: baseFileName extension: extension [
	"Assumes that the filereference instance variable is a folder and contains files named following one of the patterns supported e.g., games.22.ston or game.22
  Answer the file name with the largest number.
  If a version number is not found, raises an error"

	| matched maxVersion directory tokenizedFilename extensionString notFollowing |
	pattern ifNil: [ Error signal: 'no pattern set!' ].
	
	fileReference isDirectory
		ifFalse: [ Error signal: fileReference fullName , ' is not directory' ].
		
	directory := fileReference. 
	matched := (directory childrenMatching: baseFileName , '*', extension).
	matched isEmpty 
		ifTrue: [ Error signal: 'There is no file following the pattern ', pattern asString, ' in ', directory fullName].
	
	notFollowing := matched reject: [ :each | (each basename findTokens: '.') size = pattern size ].
	notFollowing ifNotEmpty: [ Error signal: 'Some files do not follow the pattern size.' ].
		
	maxVersion := 0.
	matched 
		do: [ :fileRef | 
			self fileReference: fileRef. 
			"this is a bit unoptimized since extract will recheck the same checks."
			maxVersion := maxVersion max: self extractVersionOrZero ].
	
	" filereference is one of the representant of the matched files"
	tokenizedFilename := fileReference basename findTokens: '.'.
	extensionString := fileReference extension = (tokenizedFilename at: (self indexOfNumber))
		ifTrue: [ '' ]
		ifFalse: [ '.' ,  fileReference extension ].
	
	 ^ ( directory / (tokenizedFilename first , '.' , maxVersion asString , extensionString )) asFileReference
]

{ #category : 'API' }
NewFileVersionner >> nextVersionFileReference [
	"Assumes a file (or folder) name follows the pattern prior.number.extension pr prior.number, 
	increment the version number and answer the new filereference. If the pattern is not set, respected or if a version number is not found, raise an error."
	

	|  tokenizedFilename version extension |
	pattern ifNil: [  Error signal: 'no pattern set!' ].

	tokenizedFilename := fileReference basename findTokens: '.'.
	(tokenizedFilename size = pattern size)
		ifFalse: [ Error new signal: fileReference fullName ,  ' structure does not follow the one of the pattern: ', pattern asString ].
	 
	version := NumberParser parse: (tokenizedFilename at: (self indexOfNumber)) onError: [ nil ].
	version ifNil: [ Error new signal: fileReference fullName ,  ' second element is not a number.'].
	
	extension := fileReference extension = (tokenizedFilename at: (self indexOfNumber))
		ifTrue: [ '' ]
		ifFalse: [ '.' ,  fileReference extension ].
		
	
	 ^ ( fileReference parent / (tokenizedFilename first , '.' , (version + 1) asString , extension )) asFileReference
]

{ #category : 'pattern' }
NewFileVersionner >> useBasenameNumber [
	
	pattern := #(basename number)
]

{ #category : 'pattern' }
NewFileVersionner >> useBasenameNumberExtension [
	
	pattern := #(basename number extension)
]

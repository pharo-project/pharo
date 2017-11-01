Extension { #name : #FileSystem }

{ #category : #'*filesystem-disk' }
FileSystem class >> * aFileOrDirectoryName [
	^ self disk * aFileOrDirectoryName
]

{ #category : #'*filesystem-disk' }
FileSystem class >> / aFileOrDirectoryName [
	^ self disk / aFileOrDirectoryName
]

{ #category : #'*filesystem-disk' }
FileSystem class >> disk [
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ DiskStore currentFileSystem
]

{ #category : #'*FileSystem-Disk' }
FileSystem >> isDiskFileSystem [
	^ store isDiskFileSystem
]

{ #category : #'*filesystem-disk' }
FileSystem class >> lookInUsualPlaces: fileName [
	"Check the default directory, the imagePath, and the vmPath (and the vmPath's owner) for this file."

	| file |
	 (file := fileName asFileReference) exists
		ifTrue: [ ^ file ].

	(file := Smalltalk imageFile resolve: fileName) exists
		ifTrue: [ ^ file ].

	(file :=  Smalltalk vmDirectory resolve: fileName) exists
		ifTrue: [ ^ file ].

	(file := Smalltalk vmDirectory parent resolve: fileName) exists
		ifTrue: [ ^ file ].

	^ nil
]

{ #category : #'*filesystem-disk' }
FileSystem class >> root [
	^ self disk root
]

{ #category : #'*filesystem-disk' }
FileSystem class >> workingDirectory [
	^ self disk workingDirectory
]

Extension { #name : #FileSystem }

{ #category : #'*Filesystem-Disk' }
FileSystem class >> * aFileOrDirectoryName [
	^ self disk * aFileOrDirectoryName
]

{ #category : #'*Filesystem-Disk' }
FileSystem class >> / aFileOrDirectoryName [
	^ self disk / aFileOrDirectoryName
]

{ #category : #'*Filesystem-Disk' }
FileSystem class >> disk [
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ DiskStore currentFileSystem
]

{ #category : #'*FileSystem-Disk' }
FileSystem >> isDiskFileSystem [
	^ store isDiskFileSystem
]

{ #category : #'*Filesystem-Disk' }
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

{ #category : #'*Filesystem-Disk' }
FileSystem class >> root [
	^ self disk root
]

{ #category : #'*Filesystem-Disk' }
FileSystem class >> workingDirectory [
	^ self disk workingDirectory
]

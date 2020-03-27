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

	fileName asFileReference ifExists: [ :file | ^ file ].

	(Smalltalk imageFile resolve: fileName) ifExists: [ :file | ^ file ].

	(Smalltalk vmDirectory resolve: fileName) ifExists: [ :file | ^ file ].

	(Smalltalk vmDirectory parent resolve: fileName) ifExists: [ :file | ^ file ].

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

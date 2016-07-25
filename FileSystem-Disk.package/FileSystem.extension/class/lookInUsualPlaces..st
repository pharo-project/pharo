*filesystem-disk
lookInUsualPlaces: fileName
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
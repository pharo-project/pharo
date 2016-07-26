public
createDirectory: path
	"Create a directory for the argument path. 
	If the path refers to an existing file, raise FileExists.
	If the path refers to an existing directory, raise DirectoryExists.
	If the parent directory of the path does not exist, raise DirectoryDoesNotExist"

	| parent encodedPathString pathString result |
	pathString := self stringFromPath: path.
	encodedPathString := Primitives encode: pathString.
	result := Primitives createDirectory: encodedPathString.
	result
		ifNil: [ 
			parent := path parent.
			(self exists: path)
				ifTrue: [ 
					(self isFile: path)
						ifTrue: [ self signalFileExists: path ]
						ifFalse: [ self signalDirectoryExists: path ] ].
			(self isDirectory: parent)
				ifFalse: [ ^ self signalDirectoryDoesNotExist: parent ].
			self primitiveFailed ].
	^ self
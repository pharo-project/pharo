"
I am raised when an operation is attempted on a file that does not exist. This includes cases where a file operation is attempted on a directory.
"
Class {
	#name : #FileDoesNotExist,
	#superclass : #FileSystemError,
	#category : #'Deprecated70-FileSystem'
}

{ #category : #deprecation }
FileDoesNotExist class >> isDeprecated [
	"Use FileDoesNotExistException instead"

	^true
]

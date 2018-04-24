"
Notify when file exists
"
Class {
	#name : #FileExistsException,
	#superclass : #FileException,
	#instVars : [
		'fileClass'
	],
	#category : #DeprecatedFileStream
}

{ #category : #exceptioninstantiator }
FileExistsException class >> fileName: aFileName fileClass: aClass [ 
	^ self new
		fileName: aFileName;
		fileClass: aClass
]

{ #category : #deprecation }
FileExistsException class >> isDeprecated [
	^true
]

{ #category : #exceptiondescription }
FileExistsException >> defaultAction [
	"The default action taken if the exception is signaled."

	^ UIManager default fileExistsDefaultAction: self

]

{ #category : #accessing }
FileExistsException >> file [
	
	^ self fileClass fileNamed: fileName
]

{ #category : #accessing }
FileExistsException >> fileClass [
	^ fileClass ifNil: [StandardFileStream]
]

{ #category : #accessing }
FileExistsException >> fileClass: aClass [
	fileClass := aClass
]

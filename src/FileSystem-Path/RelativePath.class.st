"
I represent a relative path (a position starting from Path workingDirectory)
"
Class {
	#name : #RelativePath,
	#superclass : #Path,
	#type : #variable,
	#category : #'FileSystem-Path-Base'
}

{ #category : #accessing }
RelativePath >> emptyPathString [
	"Answer the string representing an empty (size = 0) instance of the receiver.
	For a relative path, this is the current directory"

	^'.'
]

{ #category : #testing }
RelativePath >> isAbsolute [
	^ false
]

{ #category : #testing }
RelativePath >> isRoot [
	^ false
]

{ #category : #printing }
RelativePath >> printOn: aStream [

	aStream nextPutAll: 'Path '.
	self isWorkingDirectory
		ifTrue: [ aStream nextPutAll: 'workingDirectory' ]
		ifFalse: 
			[ aStream 
				nextPutAll: '* ''';
				nextPutAll: (self at: 1);
				nextPut: $'.
			2 to: self size do:
				[:i |
				aStream
					nextPutAll: ' / ''';
					nextPutAll: (self at: i);
					nextPut: $' ]]
			
]

{ #category : #printing }
RelativePath >> printOn: aStream delimiter: aCharacter [
	self isWorkingDirectory ifTrue: [aStream nextPut: $.. ^ self].
	super printOn: aStream delimiter: aCharacter
]

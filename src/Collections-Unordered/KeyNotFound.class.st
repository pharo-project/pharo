"
I am KeyNotFound, an exception indicating that a key was not found in a Dictionary.
"
Class {
	#name : #KeyNotFound,
	#superclass : #NotFound,
	#category : #'Collections-Unordered-Exceptions'
}

{ #category : #accessing }
KeyNotFound >> key [
	^ self object
]

{ #category : #accessing }
KeyNotFound >> key: aKey [
	self object: aKey
]

{ #category : #private }
KeyNotFound >> standardMessageText [
	"Generate a standard textual description"

	^ String streamContents: [ :stream |
		stream << 'key '.
		stream print: self object.
		stream << ' not found in '.
		stream print: self collection class ]
]

"
I am ValueNotFound, an exception indicating that a value was not found in a Dictionary.
"
Class {
	#name : #ValueNotFound,
	#superclass : #NotFound,
	#category : #'Collections-Unordered-Exceptions'
}

{ #category : #private }
ValueNotFound >> standardMessageText [
	"Generate a standard textual description"

	^ String streamContents: [ :stream |
		stream << 'value '.
		stream print: self object.
		stream << ' not found in '.
		stream print: self collection class]
]

{ #category : #accessing }
ValueNotFound >> value [
	^ self object
]

{ #category : #accessing }
ValueNotFound >> value: aValue [
	^ self object: aValue
]

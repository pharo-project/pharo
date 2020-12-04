"
I am a plain text. I am the most important leaf node of the document composite.
"
Class {
	#name : #PRText,
	#superclass : #PRDocumentItem,
	#instVars : [
		'text'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PRText class >> content: aString [
	^ self new text: aString
]

{ #category : #testing }
PRText class >> isAbstract [
	^ false
]

{ #category : #comparing }
PRText >> = anObject [
	^ super = anObject
		and: [ self text = anObject text ]
]

{ #category : #visiting }
PRText >> accept: aVisitor [
	^ aVisitor visitText: self
]

{ #category : #comparing }
PRText >> hash [
	^ super hash bitXor: self text hash
]

{ #category : #testing }
PRText >> isText [

	^ self class name = #PRText
]

{ #category : #printing }
PRText >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' text: '; print: self text
]

{ #category : #accessing }
PRText >> text [
	"Answer the string the receiver is representing."

	^ text
]

{ #category : #accessing }
PRText >> text: aString [
	text := aString
]

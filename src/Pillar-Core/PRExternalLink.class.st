"
I am an external link with an URL (Uniform Resource Locator) as reference.
"
Class {
	#name : #PRExternalLink,
	#superclass : #PRLink,
	#category : #'Pillar-Core-Model'
}

{ #category : #testing }
PRExternalLink class >> isAbstract [
	^ false
]

{ #category : #testing }
PRExternalLink class >> isValidReference: aString [
	^ aString includesSubstring: '://' caseSensitive: true
]

{ #category : #accessing }
PRExternalLink class >> priority [
	^ 25
]

{ #category : #visiting }
PRExternalLink >> accept: aVisitor [
	^ aVisitor visitExternalLink: self
]

{ #category : #testing }
PRExternalLink >> isExternal [
	^ true
]

{ #category : #accessing }
PRExternalLink >> localFileOrExternalReference [
	^ self class localFileOrExternalReference: self url
]

{ #category : #printing }
PRExternalLink >> printOn: aStream [

	super printOn: aStream.
	aStream space ; nextPut: $* ; nextPutAll: self url; nextPut: $*
]

{ #category : #accessing }
PRExternalLink >> url [
	"Answer the URL the receiver is pointing to."

	^ self urlPrefix , self urlEntry , self urlPostfix
]

{ #category : #'accessing - dynamic' }
PRExternalLink >> urlEntry [
	^ self reference
]

{ #category : #'accessing - dynamic' }
PRExternalLink >> urlPostfix [
	^ ''
]

{ #category : #'accessing - dynamic' }
PRExternalLink >> urlPrefix [
	^ ''
]

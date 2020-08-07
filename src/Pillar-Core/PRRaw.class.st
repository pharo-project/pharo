"
I represent a text that is not parsed/analyzed at all. I can have a type to limit the kind of output with which I'm associated. For example, an instance of me with type 'html' will only be written to HTML documents.

I'm particularly useful when the rest of Pillar can't represent something you *really* want in your output: for example, an equation. In this case, you can describe your equation in latex using an instance of me of type latex and a picture everywhere else.
"
Class {
	#name : #PRRaw,
	#superclass : #PRText,
	#instVars : [
		'type'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PRRaw class >> content: aString type: aTypeString [
	^ (self content: aString)
		type: aTypeString;
		yourself
]

{ #category : #accessing }
PRRaw class >> verbatimTypeSeparator [
	^ ':'
]

{ #category : #accessing }
PRRaw class >> verbatimTypes [
	"The verbatim types to be recognized, for now this is hardcoded."
	
	^ #('text' 'html' 'latex' 'markdown')
]

{ #category : #comparing }
PRRaw >> = anObject [
	^ super = anObject and: [ self type = anObject type ]
]

{ #category : #visiting }
PRRaw >> accept: aVisitor [
	^ aVisitor visitRaw: self
]

{ #category : #comparing }
PRRaw >> hash [
	^ super hash bitXor: self type hash
]

{ #category : #testing }
PRRaw >> isForType: aSymbol [
	^ self type isNil or: [ self type = aSymbol ]
]

{ #category : #accessing }
PRRaw >> type [
	^ type
]

{ #category : #accessing }
PRRaw >> type: aString [
	type := aString
]

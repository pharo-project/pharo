"
I am preformatted text or source code. My children are instances of *PRText*.

I'm delimited by 
= bar
= foo

in Pillar syntax.
"
Class {
	#name : #PRPreformatted,
	#superclass : #PRDocumentGroup,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRPreformatted >> accept: aVisitor [
	^ aVisitor visitPreformatted: self
]

{ #category : #'accessing - dynamic' }
PRPreformatted >> text [
	"the text of a preformatted should not be the concatenation of children."
	
	^ String streamContents: [ :stream |
		self children 
			do: [ :each | stream nextPutAll: each text ] 
			separatedBy: [ stream cr ] ]
]

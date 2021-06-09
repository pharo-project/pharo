"
I am an external link pointing to a mail address. I encode my URL to prevent spam bots collecting the address.
"
Class {
	#name : #PRMailLink,
	#superclass : #PRExternalLink,
	#category : #'Pillar-Core-Model'
}

{ #category : #testing }
PRMailLink class >> isValidReference: aString [
	| index |
	index := aString indexOf: $@ ifAbsent: [ ^ false ].
	index := aString indexOf: $. startingAt: index ifAbsent: [ ^ false ].
	^ aString allSatisfy: [ :each | each isAlphaNumeric or: [ '@-.' includes: each ] ]
]

{ #category : #accessing }
PRMailLink class >> priority [
	^ 50
]

{ #category : #visiting }
PRMailLink >> accept: aVisitor [
	^ aVisitor visitMailLink: self
]

{ #category : #'accessing - dynamic' }
PRMailLink >> scrambledUrl [
	^ self url
]

{ #category : #'accessing - dynamic' }
PRMailLink >> unscrambledUrl [
	^ self urlPrefix, super urlEntry, self urlPostfix
]

{ #category : #'accessing - dynamic' }
PRMailLink >> urlEntry [
	^ String streamContents: [ :stream |
		super urlEntry do: [ :each |
			(1 to: 2) atRandom = 1
				ifTrue: [ stream nextPut: each ]
				ifFalse: [ stream nextPut: $%; nextPutAll: ((each codePoint printStringBase: 16)
		padLeftTo: 2 with: $0) ] ] ]
]

{ #category : #'accessing - dynamic' }
PRMailLink >> urlPrefix [
	^ 'mailto:'
]

"
I represent a reference to a section, figure, environment. I follow the pattern `*@`, label, `@*`

```
See *@fig1@*
```

"
Class {
	#name : #MicAnchorReferenceBlock,
	#superclass : #MicInlineElement,
	#instVars : [
		'reference'
	],
	#category : #'Microdown-ModelInline'
}

{ #category : #testing }
MicAnchorReferenceBlock class >> isEvaluated [
	^ false
]

{ #category : #visiting }
MicAnchorReferenceBlock >> accept: aVisitor [
 	^ aVisitor visitAnchorReference: self
]

{ #category : #visiting }
MicAnchorReferenceBlock >> closingDelimiter [

 	^ MicAnchorReferenceCloserDelimiter markup
]

{ #category : #initialization }
MicAnchorReferenceBlock >> kind [

	^ #anchorReference
]

{ #category : #visiting }
MicAnchorReferenceBlock >> openingDelimiter [

 	^ MicAnchorReferenceOpenerDelimiter markup
]

{ #category : #accessing }
MicAnchorReferenceBlock >> reference [
	^ reference
]

{ #category : #accessing }
MicAnchorReferenceBlock >> reference: aReference [
	reference := aReference
]

{ #category : #visiting }
MicAnchorReferenceBlock >> referenceAsHTML [	
	| ref |
	self flag: #todo.
	ref := self reference.
	(ref endsWith: '.mic')
		ifTrue: [ ref := (ref copyUpToLast: $.) , '.html' ]
		ifFalse: [ Error signal: 'Reference : ' , ref , ' should end with .pillar or .pier' ].
	^ ref
]

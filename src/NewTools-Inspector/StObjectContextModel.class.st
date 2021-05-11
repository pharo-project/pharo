"
A model to be used with `StObjectContextPresenter`.
It keeps references to: 

- the inspector model
- the inspection context
- the inspection object
"
Class {
	#name : #StObjectContextModel,
	#superclass : #Object,
	#instVars : [
		'inspection',
		'context',
		'inspectedObject'
	],
	#classVars : [
		'CodeSnippets'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #private }
StObjectContextModel class >> codeSnippetFor: aClass ifAbsent: aBlock [

	^ self codeSnippets 
		at: aClass
		ifAbsent: aBlock
]

{ #category : #private }
StObjectContextModel class >> codeSnippetFor: aClass value: aString [

	self codeSnippets 	
		at: aClass 
		put: aString
]

{ #category : #private }
StObjectContextModel class >> codeSnippets [

	^ CodeSnippets ifNil: [ CodeSnippets := WeakIdentityKeyDictionary new ]
]

{ #category : #accessing }
StObjectContextModel >> codeSnippet [

	^ self class 
		codeSnippetFor: self inspectedObject class
		ifAbsent: [ 'self' ]
]

{ #category : #accessing }
StObjectContextModel >> codeSnippet: aString [

	(self stringWithoutInitialCommentFrom: aString asString) 
		ifNotEmpty: [ :snippet |
			self class 
				codeSnippetFor: self inspectedObject class
				value: snippet ]
]

{ #category : #accessing }
StObjectContextModel >> context [
	^ context
]

{ #category : #accessing }
StObjectContextModel >> context: anObject [
	context := anObject
]

{ #category : #testing }
StObjectContextModel >> hasEvaluator [
	
	^ self context hasEvaluator
]

{ #category : #accessing }
StObjectContextModel >> inspectedObject [
	^ inspectedObject
]

{ #category : #accessing }
StObjectContextModel >> inspectedObject: anObject [
	inspectedObject := anObject
]

{ #category : #accessing }
StObjectContextModel >> inspection [
	^ inspection
]

{ #category : #accessing }
StObjectContextModel >> inspection: anObject [
	inspection := anObject
]

{ #category : #factory }
StObjectContextModel >> newInspectionView [

	^ self context newInspectionView
]

{ #category : #accessing }
StObjectContextModel >> smallDescriptionString [

	^ self inspection smallDescriptionStringFor: self inspectedObject
]

{ #category : #'private building' }
StObjectContextModel >> stringWithoutInitialCommentFrom: aString [

	^ (aString trimLeft notEmpty and: [ aString trimLeft first = $" ])
		ifFalse: [ aString trimRight ]
		ifTrue: [ | startIndex doubleQuoteCounter |
			startIndex := aString findAnySubstring: '"' startingAt: (aString indexOf: $") + 1.
			doubleQuoteCounter := 1.
			"we need to iterate over the whole string queuing quotes to support nested double quotes"
			[ startIndex <= aString size
				and: [ doubleQuoteCounter % 2 = 1
					or: [ (aString at: startIndex) = $"
						or: [ (aString at: startIndex) isSeparator ] ] ] ]
			whileTrue: [ 
				(aString at: startIndex) = $" ifTrue: [ 
					doubleQuoteCounter := doubleQuoteCounter + 1 ].
				startIndex := startIndex + 1 ].
	
			(startIndex >= (aString size + 1)) ifTrue: [ "No content if a comment cannot be identified. " ^ '' ].
			(aString
				copyFrom: startIndex
				to: aString size) trimBoth ]
]

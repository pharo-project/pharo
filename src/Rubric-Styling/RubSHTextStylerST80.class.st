"
I extend SHTextStylerST80 and replace the links for methdos, classes and variables with the Rubric specific links.
"
Class {
	#name : #RubSHTextStylerST80,
	#superclass : #SHRBTextStyler,
	#category : #'Rubric-Styling'
}

{ #category : #'style table' }
RubSHTextStylerST80 class >> initialTextAttributesForPixelHeight: aNumber [ 
	^ SHTextStylerST80 initialTextAttributesForPixelHeight: aNumber 
]

{ #category : #initialization }
RubSHTextStylerST80 class >> initialize [
	"self initialize"
	styleTable := nil.
	textAttributesByPixelHeight := nil.
]

{ #category : #accessing }
RubSHTextStylerST80 class >> styleTable [
	^ SHTextStylerST80 styleTable
]

{ #category : #accessing }
RubSHTextStylerST80 class >> styleTable: anArray [ 
	SHTextStylerST80 styleTable: anArray 
]

{ #category : #accessing }
RubSHTextStylerST80 class >> textAttributesByPixelHeight [  
	^ SHTextStylerST80 textAttributesByPixelHeight 
]

{ #category : #'visiting rb nodes' }
RubSHTextStylerST80 >> visitMessageNode: aMessageNode [
	| style link |
	style := #keyword.
	(Symbol findInternedSelector: aMessageNode selector asString)
		ifNil: [ 
			style := (Symbol selectorThatStartsCaseSensitive: aMessageNode selector asString skipping: nil) isNil
				ifTrue: [ #undefinedKeyword ]
				ifFalse: [ #incompleteKeyword ] ].
	link := TextMethodLink sourceNode: aMessageNode.
	self styleOpenParenthese: aMessageNode.
	aMessageNode selectorParts
		with: aMessageNode keywordsPositions
		do: [ :keyword :position | 
			self
				addStyle: style
				attribute: link
				from: position
				to: position + keyword size - 1 ].
	(aMessageNode isCascaded not or: [ aMessageNode isFirstCascaded ])
		ifTrue: [ self visitNode: aMessageNode receiver ].
	aMessageNode arguments do: [ :each | self visitNode: each ].
	self styleCloseParenthese: aMessageNode
]

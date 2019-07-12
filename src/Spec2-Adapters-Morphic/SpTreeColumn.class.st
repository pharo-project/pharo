"
I am a Tree column made especially to be used by the Spec framework
"
Class {
	#name : #SpTreeColumn,
	#superclass : #MorphTreeColumn,
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #compatibility }
SpTreeColumn >> bindKeyCombination: aKMCombination toAction: aBlock [

	"Do nothing"
	
]

{ #category : #'column drawing' }
SpTreeColumn >> rowMorphFor: aNode [ 
	| node rowMorph |
	
	node := aNode nodeModel.
	rowMorph := self rowMorphGetSelector
		ifNil: [node rowMorphForColumn: self]
		ifNotNil: [self rowMorphGetterBlock value: node value: self container].
	rowMorph := rowMorph asMorph.
	rowMorph
		ifNotNil: [rowMorph borderWidth: 0]
		ifNil: [rowMorph := self defaultRowMorph].
	^ rowMorph rowMorphForNode: node inColumn: self

]

{ #category : #private }
SpTreeColumn >> setHeaderButtonLabel: aLabel font: aFont icon: anIconForm target: aReceiver actionSelector: aSelector [
	(aLabel isNil
		and: [ anIconForm isNil and: [ aReceiver isNil or: [ aSelector isNil ] ] ])
		ifTrue: [ ^ self ].
	self headerButton.
	aLabel
		ifNil: [ self header label: '' font: nil ]
		ifNotNil: [ self header label: aLabel font: aFont ].
	anIconForm
		ifNotNil: [ self header icon: (ImageMorph new form: anIconForm) ].
	(aSelector notNil and: [ aReceiver notNil ])
		ifFalse: [ ^ self ].
	self header actionSelector: aSelector.
	self header target: aReceiver
]

{ #category : #accessing }
SpTreeColumn >> visible: aBoolean [

	"Do nothing. Morphs implement visible but this column, though it inherits from Morphic column and has an adapter, dos not."
]

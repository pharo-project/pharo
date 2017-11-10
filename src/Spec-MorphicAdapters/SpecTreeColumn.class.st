"
I am a Tree column made especially to be used by the Spec framework
"
Class {
	#name : #SpecTreeColumn,
	#superclass : #MorphTreeColumn,
	#category : #Spec-MorphicAdapters-Support
}

{ #category : #compatibility }
SpecTreeColumn >> bindKeyCombination: aKMCombination toAction: aBlock [

	"Do nothing"
	
]

{ #category : #'column drawing' }
SpecTreeColumn >> rowMorphFor: aNode [ 
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
SpecTreeColumn >> setHeaderButtonLabel: aLabel font: aFont icon: anIconForm target: aReceiver actionSelector: aSelector [
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

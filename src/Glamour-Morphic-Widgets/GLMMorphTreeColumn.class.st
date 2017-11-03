Class {
	#name : #GLMMorphTreeColumn,
	#superclass : #MorphTreeColumn,
	#instVars : [
		'glamourColumn'
	],
	#category : #Glamour-Morphic-Widgets
}

{ #category : #accessing }
GLMMorphTreeColumn >> glamourColumn: aColumn [
	glamourColumn := aColumn
]

{ #category : #'column drawing' }
GLMMorphTreeColumn >> rowMorphFor: aNodeMorph [
	| rowMorph cont |
	
	rowMorph := aNodeMorph rowMorphForColumn: glamourColumn.
	rowMorph
		ifNotNil: [rowMorph borderWidth: 0]
		ifNil: [rowMorph := self defaultRowMorph].
	cont := Morph new color: Color transparent;
				 layoutPolicy: RowLayout new;
				 hResizing: #shrinkWrap;
				 vResizing: #shrinkWrap;
				 layoutInset: container columnInset @ container rowInset;
				 yourself.
	cont addMorph: rowMorph.

	self isFirstColumn ifTrue: [ | icon |
		icon := container iconBlock value: aNodeMorph.
		icon ifNotNil: [ cont addMorph: icon asMorph ].
		"for first column we don't use horizontal inset"
		cont  layoutInset: 0 @ container rowInset.
	].
	^ cont 
]

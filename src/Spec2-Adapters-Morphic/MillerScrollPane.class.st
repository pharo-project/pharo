Class {
	#name : #MillerScrollPane,
	#superclass : #GeneralScrollPane,
	#instVars : [
		'layoutChangeAction',
		'updating'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #initialization }
MillerScrollPane >> initialize [

	updating := false.	
	super initialize
]

{ #category : #layout }
MillerScrollPane >> layoutChanged [

	updating ifTrue: [ ^ self ].
	updating := true.
	[
		super layoutChanged.
		layoutChangeAction ifNotNil: [ layoutChangeAction value ] ]
	ensure: [ 
		updating := false ]
]

{ #category : #'as yet unclassified' }
MillerScrollPane >> onLayoutChange: aBlock [

	layoutChangeAction := aBlock
]

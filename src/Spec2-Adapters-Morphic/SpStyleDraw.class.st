"
! Draw property (==SpStyleDraw==)

Draw properties control how the component (morph) will be draw.
I keep this properties: 

- color: foreground color for the morph if it applies (if the morph understands #color:).
- backgroundColor: background color if it applies (if the morph understands #backgroundColor:).

See *method:SpStyleDraw#color* and *method:SpStyleDraw#backgroundColor*

!! Usage
The identifier of draw in the stylesheet is ==Draw==.

[[[
Draw { 
	#color: #red,
	#backgroundColor: '00FF00'
}
]]]
"
Class {
	#name : #SpStyleDraw,
	#superclass : #SpStyleProperty,
	#instVars : [
		'color',
		'backgroundColor'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleDraw class >> stonName [

	^ 'Draw'
]

{ #category : #operations }
SpStyleDraw >> applyTo: aMorph [ 

	self color ifNotNil: [ :aColor | aMorph color: aColor ].
	self backgroundColor ifNotNil: [ :aColor |
		"hack to be able to decorate components that does not understand 
		 backgroundColor" 
		(aMorph respondsTo: #backgroundColor:)
			ifTrue: [ aMorph backgroundColor: aColor ]
			ifFalse: [ aMorph fillStyle: aColor ] ].
	
]

{ #category : #accessing }
SpStyleDraw >> backgroundColor [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ backgroundColor
]

{ #category : #accessing }
SpStyleDraw >> backgroundColor: aColorOrSymbol [

	backgroundColor := self toColor: aColorOrSymbol
]

{ #category : #accessing }
SpStyleDraw >> color [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ color
]

{ #category : #accessing }
SpStyleDraw >> color: aColorOrSymbol [

	color := self toColor: aColorOrSymbol
]

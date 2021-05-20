"
! Draw property (`SpDrawStyle`)

Draw properties control how the component (morph) will be draw.
I keep this properties: 

- color: foreground color for the morph if it applies (if the morph understands #color:).
- backgroundColor: background color if it applies (if the morph understands #backgroundColor:).

See `SpDrawStyle>>#color` and `SpDrawStyle>>#backgroundColor`

## Usage
The identifier of draw in the stylesheet is `Draw`.

```
Draw { 
	#color: #red,
	#backgroundColor: '00FF00'
}
```
"
Class {
	#name : #SpDrawStyle,
	#superclass : #SpPropertyStyle,
	#instVars : [
		'color',
		'backgroundColor'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpDrawStyle class >> stonName [

	^ 'Draw'
]

{ #category : #operations }
SpDrawStyle >> applyTo: aMorph [ 

	self color ifNotNil: [ :aColor | aMorph color: aColor asStyleVariable valueAsColor ].
	self backgroundColor ifNotNil: [ :aColor |
		"hack to be able to decorate components that does not understand 
		 backgroundColor" 
		(aMorph respondsTo: #backgroundColor:)
			ifTrue: [ aMorph backgroundColor: aColor asStyleVariable valueAsColor ]
			ifFalse: [ aMorph fillStyle: aColor asStyleVariable valueAsColor ] ]
]

{ #category : #accessing }
SpDrawStyle >> backgroundColor [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self backgroundColorVariable value
]

{ #category : #accessing }
SpDrawStyle >> backgroundColor: aColorOrSymbol [

	backgroundColor := aColorOrSymbol asStyleVariable
]

{ #category : #'accessing variables' }
SpDrawStyle >> backgroundColorVariable [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self toVariable: backgroundColor
]

{ #category : #accessing }
SpDrawStyle >> color [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self colorVariable value
]

{ #category : #accessing }
SpDrawStyle >> color: aColorOrSymbol [

	color := aColorOrSymbol asStyleVariable
]

{ #category : #'accessing variables' }
SpDrawStyle >> colorVariable [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self toVariable: color
]

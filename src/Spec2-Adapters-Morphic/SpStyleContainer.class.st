"
! Container properties (==SpStyleContainer==)

A container property can be applied to container elements (buttonbar, toolbar, actionbar), and define several properties: 

- borderColor: The color of the border (in case borderWidth > 0). 
- borderWidth: The width of the border.
- padding: The space between elements.

See *method:SpStyleContainer#borderColor*

!! Usage
The identifier of container in the stylesheet is ==Container==.

[[[
Container { 
	#borderColor: #blue, 
	#borderWidth: 2,
	#padding: 5
}
]]]
"
Class {
	#name : #SpStyleContainer,
	#superclass : #SpStyleProperty,
	#instVars : [
		'borderWidth',
		'padding',
		'borderColor'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpStyleContainer class >> stonName [

	^ 'Container'
]

{ #category : #operations }
SpStyleContainer >> applyTo: aMorph [ 

	self borderColor ifNotNil: [ :aColor | aMorph borderColor: aColor ].
	self borderWidth ifNotNil: [ :aNumber | aMorph borderWidth: aNumber ].
	self padding ifNotNil: [ :aNumber | aMorph cellInset: aNumber ]
]

{ #category : #accessing }
SpStyleContainer >> borderColor [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ borderColor
]

{ #category : #accessing }
SpStyleContainer >> borderColor: aColor [

	borderColor := aColor
]

{ #category : #accessing }
SpStyleContainer >> borderWidth [

	^ borderWidth
]

{ #category : #accessing }
SpStyleContainer >> borderWidth: aNumber [

	borderWidth := aNumber
]

{ #category : #accessing }
SpStyleContainer >> padding [

	^ padding
]

{ #category : #accessing }
SpStyleContainer >> padding: aNumber [

	padding := aNumber
]

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

	self borderColorVariable ifNotNil: [ :aVariable | aMorph borderColor: aVariable valueAsColor ].
	self borderWidthVariable ifNotNil: [ :aVariable | aMorph borderWidth: aVariable value ].
	self paddingVariable ifNotNil: [ :aVariable | aMorph cellInset: aVariable value ]
]

{ #category : #accessing }
SpStyleContainer >> borderColor [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self borderColorVariable value
]

{ #category : #accessing }
SpStyleContainer >> borderColor: aColor [

	borderColor := aColor asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleContainer >> borderColorVariable [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self toVariable: borderColor
]

{ #category : #accessing }
SpStyleContainer >> borderWidth [

	^ self borderWidthVariable value
]

{ #category : #accessing }
SpStyleContainer >> borderWidth: aNumber [

	borderWidth := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleContainer >> borderWidthVariable [

	^ self toVariable: borderWidth
]

{ #category : #accessing }
SpStyleContainer >> padding [

	^ self paddingVariable value
]

{ #category : #accessing }
SpStyleContainer >> padding: aNumber [

	padding := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpStyleContainer >> paddingVariable [

	^ self toVariable: padding
]

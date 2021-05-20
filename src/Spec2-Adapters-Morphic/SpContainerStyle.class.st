"
# Container properties (`SpContainerStyle`)

A container property can be applied to container elements (buttonbar, toolbar, actionbar), and define several properties: 

- borderColor: The color of the border (in case borderWidth > 0). 
- borderWidth: The width of the border.
- padding: The space between elements.

See `SpContainerStyle>>#borderColor`

## Usage
The identifier of container in the stylesheet is`Container`.

```
Container { 
	#borderColor: #blue, 
	#borderWidth: 2,
	#padding: 5
}
```
"
Class {
	#name : #SpContainerStyle,
	#superclass : #SpPropertyStyle,
	#instVars : [
		'borderWidth',
		'padding',
		'borderColor'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'ston-core' }
SpContainerStyle class >> stonName [

	^ 'Container'
]

{ #category : #operations }
SpContainerStyle >> applyTo: aMorph [ 

	self borderColorVariable ifNotNil: [ :aVariable | aMorph borderColor: aVariable valueAsColor ].
	self borderWidthVariable ifNotNil: [ :aVariable | aMorph borderWidth: aVariable value ].
	self paddingVariable ifNotNil: [ :aVariable | aMorph cellInset: aVariable value ]
]

{ #category : #accessing }
SpContainerStyle >> borderColor [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self borderColorVariable value
]

{ #category : #accessing }
SpContainerStyle >> borderColor: aColor [

	borderColor := aColor asStyleVariable
]

{ #category : #'accessing variables' }
SpContainerStyle >> borderColorVariable [
  "This property can be expressed as 
	- a STON map: ==Color { #red : 1., #green : 0, #blue : 0, #alpha : 1 }==
	- a named selector: ==#red==
	- an hex string: =='FF0000'=="

	^ self toVariable: borderColor
]

{ #category : #accessing }
SpContainerStyle >> borderWidth [

	^ self borderWidthVariable value
]

{ #category : #accessing }
SpContainerStyle >> borderWidth: aNumber [

	borderWidth := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpContainerStyle >> borderWidthVariable [

	^ self toVariable: borderWidth
]

{ #category : #accessing }
SpContainerStyle >> padding [

	^ self paddingVariable value
]

{ #category : #accessing }
SpContainerStyle >> padding: aNumber [

	padding := aNumber asStyleVariable
]

{ #category : #'accessing variables' }
SpContainerStyle >> paddingVariable [

	^ self toVariable: padding
]

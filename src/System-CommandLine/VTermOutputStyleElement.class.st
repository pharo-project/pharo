"
I represent a style element of a VTermOutputStyle.
I can change my color
"
Class {
	#name : #VTermOutputStyleElement,
	#superclass : #Object,
	#instVars : [
		'color'
	],
	#pools : [
		'VTermOutputStyles'
	],
	#category : #System-CommandLine
}

{ #category : #'text coloring' }
VTermOutputStyleElement >> black [
	"Change the current color of writing"

	self color: Color black
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> black: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color black
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> blue [
	"Change the current color of writing"

	self color: Color blue
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> blue: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color blue
]

{ #category : #accessing }
VTermOutputStyleElement >> color [

	^ color
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> color: aColor [
	"Change the current color of writing"

	color := aColor.
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> cyan [
	"Change the current color of writing"

	self color: Color cyan
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> cyan: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color cyan
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> green [
	"Change the current color of writing"

	self color: Color green
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> green: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color green
]

{ #category : #initialization }
VTermOutputStyleElement >> initialize [

	super initialize.
	color := VTermDefaultColor new
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> magenta [
	"Change the current color of writing"

	self color: Color magenta
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> magenta: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color magenta
]

{ #category : #copying }
VTermOutputStyleElement >> postCopy [
	color := self color copy.
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> red [
	"Change the current color of writing"

	self color: Color red
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> red: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color red
]

{ #category : #styling }
VTermOutputStyleElement >> setInDriver: aVTermOutputDriver [ 
	self subclassResponsibility
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> white [
	"Change the current color of writing"

	self color: Color white
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> white: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color white
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> yellow [
	"Change the current color of writing"

	self color: Color yellow
]

{ #category : #'text coloring' }
VTermOutputStyleElement >> yellow: aString [
	"Change the color of writing of aString and set it back to default color afterward"

	self print: aString withColor: Color yellow
]

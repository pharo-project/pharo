"
I represent a style element of a VTermOutputStyle.
I can change my color
"
Class {
	#name : 'VTermOutputStyleElement',
	#superclass : 'Object',
	#instVars : [
		'color'
	],
	#pools : [
		'VTermOutputStyles'
	],
	#category : 'System-CommandLine-VTerm-Styling',
	#package : 'System-CommandLine',
	#tag : 'VTerm-Styling'
}

{ #category : 'text coloring' }
VTermOutputStyleElement >> black [
	"Change the current color of writing"

	self color: Color black
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> blue [
	"Change the current color of writing"

	self color: Color blue
]

{ #category : 'accessing' }
VTermOutputStyleElement >> color [

	^ color
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> color: aColor [
	"Change the current color of writing"

	color := aColor
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> cyan [
	"Change the current color of writing"

	self color: Color cyan
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> green [
	"Change the current color of writing"

	self color: Color green
]

{ #category : 'initialization' }
VTermOutputStyleElement >> initialize [

	super initialize.
	color := VTermDefaultColor new
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> magenta [
	"Change the current color of writing"

	self color: Color magenta
]

{ #category : 'copying' }
VTermOutputStyleElement >> postCopy [
	color := self color copy
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> red [
	"Change the current color of writing"

	self color: Color red
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> white [
	"Change the current color of writing"

	self color: Color white
]

{ #category : 'text coloring' }
VTermOutputStyleElement >> yellow [
	"Change the current color of writing"

	self color: Color yellow
]

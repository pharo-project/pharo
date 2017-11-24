"
A GLMStacker is an explicit browser that allows us to stack panes on top of each other and typically show them as tabs.
"
Class {
	#name : #GLMStacker,
	#superclass : #GLMExplicitBrowser,
	#category : #'Glamour-Browsers-Other'
}

{ #category : #scripting }
GLMStacker >> aPane: aBlockOrSymbol [
	^ self addNewPaneNamed: aBlockOrSymbol
]

{ #category : #scripting }
GLMStacker >> panes: blocksOrSymbols [
	^ blocksOrSymbols do: [:each | self aPane: each]
]

{ #category : #rendering }
GLMStacker >> renderGlamorouslyOn: aRenderer [ 
	^aRenderer renderStacker: self
]

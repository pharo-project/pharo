"
I am the adapter used to bridge a WorldPresenter and a WorldMorph
"
Class {
	#name : #SpMorphicWorldAdapter,
	#superclass : #SpAbstractMorphicWindowAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #adding }
SpMorphicWorldAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [

	self 
		addContent: (self model presenter buildWithSpecLayout: aSpec) 
		toWindow: widgetToBuild
]

{ #category : #factory }
SpMorphicWorldAdapter >> buildWidget [

	^ self presenter currentWorld
		borderColor: self theme backgroundColor;
		borderWidth: 4;
		yourself
]

{ #category : #api }
SpMorphicWorldAdapter >> centered [
	"Do nothing. I will fill the full world"
]

{ #category : #private }
SpMorphicWorldAdapter >> newContainerMorph [
	^ super newContainerMorph 
		adoptPaneColor: self theme backgroundColor;
		yourself
]

{ #category : #private }
SpMorphicWorldAdapter >> title: aString [
	"Update the host window title"

	DisplayScreen hostWindowTitle: aString
]

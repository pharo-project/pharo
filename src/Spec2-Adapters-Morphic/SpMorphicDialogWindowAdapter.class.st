"
I am the adapter used to bridge a DialogWindowPresenter and a DialogWindow
"
Class {
	#name : #SpMorphicDialogWindowAdapter,
	#superclass : #SpMorphicWindowAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #private }
SpMorphicDialogWindowAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [
	"I replace the mainPanel (which contains contents and button bar) because like that I get the 
	 status bar at the end (where it belongs)"

	super addPresenterIn: widgetToBuild withSpecLayout: aSpec.
	self model buttons ifNotEmpty: [ 	
		widgetToBuild setToolbarFrom: [ self buildButtonBar ] ]
]

{ #category : #factory }
SpMorphicDialogWindowAdapter >> buildButtonBar [
	| buttonBar |
	
	buttonBar := (SpButtonBarPresenter owner: self model) placeAtEnd.
	self model buttons do: [ :buttonPresenter | 
		buttonBar add: buttonPresenter ].
	
	^ buttonBar
]

{ #category : #factory }
SpMorphicDialogWindowAdapter >> buildWidget [
	^ SpDialogWindow new
		model: self model;
		yourself
]

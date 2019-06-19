"
I am the adapter used to bridge a DialogWindowPresenter and a DialogWindow
"
Class {
	#name : #MorphicDialogWindowAdapter,
	#superclass : #MorphicWindowAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #private }
MorphicDialogWindowAdapter >> addPresenterIn: widgetToBuild withSpecLayout: aSpec [
	"I replace the mainPanel (which contains contents and button bar) because like that I get the 
	 status bar at the end (where it belongs)"

	super addPresenterIn: widgetToBuild withSpecLayout: aSpec.
	self model buttons ifNotEmpty: [ 	
		widgetToBuild setToolbarFrom: [ self buildButtonBar ] ]
]

{ #category : #factory }
MorphicDialogWindowAdapter >> buildButtonBar [
	| buttonBar |
	
	buttonBar := (ButtonBarPresenter owner: self model) placeAtEnd.
	self model buttons do: [ :buttonPresenter | 
		buttonBar add: buttonPresenter ].
	
	^ buttonBar
]

{ #category : #factory }
MorphicDialogWindowAdapter >> buildWidget [
	^ SpecDialogWindow new
		model: self model;
		yourself
]

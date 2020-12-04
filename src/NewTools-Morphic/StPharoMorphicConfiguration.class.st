Class {
	#name : #StPharoMorphicConfiguration,
	#superclass : #SpApplicationConfiguration,
	#category : #'NewTools-Morphic'
}

{ #category : #defaults }
StPharoMorphicConfiguration class >> defaultStyleSheet [

	^ SpStyle defaultStyleSheet
]

{ #category : #configuring }
StPharoMorphicConfiguration >> configure: anApplication [

	super configure: anApplication
]

{ #category : #configuring }
StPharoMorphicConfiguration >> configureOSX: anApplication [
	
	anApplication styleSheet: (SpStyle defaultStyleSheet, self styleSheetOSX)
	
]

{ #category : #configuring }
StPharoMorphicConfiguration >> configureUnix: anApplication [
	
	anApplication styleSheet: SpStyle defaultStyleSheet
]

{ #category : #configuring }
StPharoMorphicConfiguration >> configureWindows: anApplication [
	
	anApplication styleSheet: SpStyle defaultStyleSheet
]

{ #category : #private }
StPharoMorphicConfiguration >> styleSheetOSX [
	"Just an example on how to build styles programatically ;)"

	^ SpStyle newApplication 
		addClass: 'label' with: [ :label |
			label addClass: 'shortcut' with: [ :shortcut |
				shortcut addPropertyFontWith: [ :font |
					font name: 'Lucida Grande'; size: 10 ] ] ]
]

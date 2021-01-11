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
	
	anApplication styleSheet: (self styleSheet, self styleSheetOSX)
	
]

{ #category : #configuring }
StPharoMorphicConfiguration >> configureUnix: anApplication [
	
	anApplication styleSheet: self styleSheet
]

{ #category : #configuring }
StPharoMorphicConfiguration >> configureWindows: anApplication [
	
	anApplication styleSheet: self styleSheet
]

{ #category : #private }
StPharoMorphicConfiguration >> styleSheet [

	^ SpStyle defaultStyleSheet, self styleSheetCommon
]

{ #category : #private }
StPharoMorphicConfiguration >> styleSheetCommon [
	"Just an example on how to build styles programatically ;)"

	^ SpStyle newApplication 
		addClass: 'searchInputField' with: [ :searchInputField |
			searchInputField addClass: 'stSpotter' with: [ :spotter |
				spotter 
					addPropertyGeometryWith: [ :geometry | geometry height: 30 ];
					addPropertyFontWith: [ :font | font size: 12 ] ] ];
		yourself
]

{ #category : #private }
StPharoMorphicConfiguration >> styleSheetOSX [
	"Just an example on how to build styles programatically ;)"

	^ SpStyle newApplication 
		addClass: 'label' with: [ :label |
			label addClass: 'shortcut' with: [ :shortcut |
				shortcut addPropertyFontWith: [ :font |
					font name: 'Lucida Grande'; size: 10 ] ] ];
		yourself
]

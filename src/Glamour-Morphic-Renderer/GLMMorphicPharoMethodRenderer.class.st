Class {
	#name : #GLMMorphicPharoMethodRenderer,
	#superclass : #GLMMorphicPharoCodeRenderer,
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #private }
GLMMorphicPharoMethodRenderer >> methodBindingsStrategy [

	^ [ :currentBindings :aSymbol |
		 currentBindings associationAt: aSymbol ifAbsent: [nil] ]
]

{ #category : #rendering }
GLMMorphicPharoMethodRenderer >> modelFor: aPresentation [

	^ (super modelFor: aPresentation)
		bindingsStrategy: self methodBindingsStrategy;
		appendVariableBinding: aPresentation variableBindings;
		yourself
]

{ #category : #rendering }
GLMMorphicPharoMethodRenderer >> morph [
	|morph|
	morph := RubScrolledTextMorph new
		beForSmalltalkCode;
		getSelectionSelector: #primarySelectionInterval;
		model: textModel;
		color: self theme backgroundColor;
		textFont: StandardFonts codeFont;
		yourself.
	
	(morph textArea decoratorNamed: #shoutStyler) ifNotNil: [ :styler | 
		"In case a method presentation has variable bindings change the settings
		of the styler to take this into account: set the current model as a workspace
		in the styler and inform it the code represents a method."
		textModel glamourPresentation variableBindings ifNotEmpty: [
			styler styler 
				workspace: textModel;
				isForWorkspace: false. 
			styler refreshStyling] ].
	^ morph
]

Class {
	#name : #GLMMorphicPharoScriptRenderer,
	#superclass : #GLMMorphicPharoCodeRenderer,
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #callbacks }
GLMMorphicPharoScriptRenderer >> actOnVariableBindingsAccessRequest: ann [ 

	ann action ifNotNil: [ :aBlock |
		aBlock cull: textModel variableBindings ]
]

{ #category : #callbacks }
GLMMorphicPharoScriptRenderer >> actOnVariableBindingsUpdated: ann [ 

	textModel variableBindings: nil.
	textModel appendVariableBinding: ann presentation variableBindings.
	textMorph shoutStyler style: textMorph text.
]

{ #category : #rendering }
GLMMorphicPharoScriptRenderer >> modelFor: aPresentation [

	^ (super modelFor: aPresentation)
		highlightSmalltalk: true;
		bindingsStrategy: self workspaceBindingsStrategy;
		appendVariableBinding: aPresentation variableBindings;
		yourself
]

{ #category : #rendering }
GLMMorphicPharoScriptRenderer >> morph [
	|morph|
	morph := RubScrolledTextMorph new
		getSelectionSelector: #primarySelectionInterval;
		model: textModel;
		beForSmalltalkScripting;
		textFont: StandardFonts codeFont;
		color: self theme backgroundColor;
		notificationStrategy: GLMPopperNotificationStrategy new;
		yourself.
	"Without forcing styling explicitly the first time the presentation is opened with some text on it the styling is wrong."
	(morph textArea decoratorNamed: #shoutStyler) ifNotNil: [ :styler | styler refreshStyling ].
	^ morph
]

{ #category : #private }
GLMMorphicPharoScriptRenderer >> registerEventsForPresentation: aPresentation [

	super registerEventsForPresentation: aPresentation.
	
	aPresentation when: GLMVariableBindingsChanged send: #actOnVariableBindingsUpdated: to: self.
	aPresentation when: GLMVariableBindingsAccessRequest send: #actOnVariableBindingsAccessRequest: to: self.
	
]

{ #category : #private }
GLMMorphicPharoScriptRenderer >> workspaceBindingsStrategy [

	^ [ :currentBindings :aString :aTextModel |
		(aTextModel doItReceiver class allInstVarNames includes: aString) 
			ifTrue: [ nil ]
			ifFalse: [  	
				(currentBindings includesKey: aString)
					ifFalse: [currentBindings add: (WorkspaceVariable key: aString asSymbol) ].
				currentBindings associationAt: aString] ]
]

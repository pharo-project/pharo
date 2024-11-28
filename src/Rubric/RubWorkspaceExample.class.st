"
self open
"
Class {
	#name : 'RubWorkspaceExample',
	#superclass : 'Object',
	#instVars : [
		'textModel',
		'announcer',
		'bindings',
		'mustDeclareVariables'
	],
	#category : 'Rubric-Editing-Widgets-Examples',
	#package : 'Rubric',
	#tag : 'Editing-Widgets-Examples'
}

{ #category : 'examples' }
RubWorkspaceExample class >> exampleWindowWithScrolledText [
	<sampleInstance>

	^self open
]

{ #category : 'examples' }
RubWorkspaceExample class >> exampleWithCustomShortcut [

	| window text |
	text := RubScrolledTextMorph new.

	"define a custom shortcut  - notice  the call to textArea"

	text textArea removeKeyCombination: $s meta.
	text setText: 'Hit cmd $s to revert the text'.
	text textArea
		bindKeyCombination: $s meta
		toActions: [ text setText: text text asString reverse ].

	window := StandardWindow new.
	window addMorph: text fullFrame: LayoutFrame identity.
	window title: 'Custom shortcuts example'.
	window openInWorld
]

{ #category : 'instance creation' }
RubWorkspaceExample class >> open [
	<script>
	(self windowWithScrolledText: (self new newScrolledText)) openInWorld
]

{ #category : 'private - accessing' }
RubWorkspaceExample class >> title [
	^ 'Rubric workspace example'
]

{ #category : 'private - utilities' }
RubWorkspaceExample class >> windowWithScrolledText: aScrolledText [
	| w |
	w := StandardWindow new.
	w title: self title.
	w addMorph: aScrolledText fullFrame: LayoutFrame identity.
	^w
]

{ #category : 'accessing' }
RubWorkspaceExample >> acceptString: aString [
	textModel setText: aString asText.
	 ^ true
]

{ #category : 'accessing' }
RubWorkspaceExample >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : 'shout' }
RubWorkspaceExample >> bindingOf: aString [

	mustDeclareVariables
		ifTrue: [ ^ nil ].	"I want to have workspace that force the user to declare
	variables. Still subclasses may want to do something else"
	bindings ifNil: [ self initializeBindings ].
	( bindings includesKey: aString )
		ifFalse: [ bindings at: aString put: nil ].
	^ bindings associationAt: aString
]

{ #category : 'code pane menu' }
RubWorkspaceExample >> codePaneMenu: aMenu shifted: shifted [
	"Note that unless we override perform:orSendTo:,
	PluggableTextController will respond to all menu items in a
	text pane"
	"Not tested"
]

{ #category : 'user interface' }
RubWorkspaceExample >> getText [
	^ textModel getText
]

{ #category : 'shout' }
RubWorkspaceExample >> hasBindingOf: aString [
	^bindings includesKey: aString asSymbol
]

{ #category : 'initialization' }
RubWorkspaceExample >> initialize [
	super initialize.
	textModel := RubScrolledTextModel new
		interactionModel: self;
		yourself.
	mustDeclareVariables := false
]

{ #category : 'user interface' }
RubWorkspaceExample >> newScrolledText [
	| st |
	st := textModel newScrolledText
		beWrapped;
		autoAccept: true;
		withLineNumbers;
		withGhostText: 'Workspace: a place where you can code and test scripts';
		withAnnotation;
		yourself.
	st ghostTextRuler fontName: 'Bitmap DejaVu Sans' pointSize: 18; textColor: Color gray lighter; centered.
	^st
]

{ #category : 'user interface' }
RubWorkspaceExample >> setText: aText from: aView [
	^ self acceptString: aText string
]

{ #category : 'shout' }
RubWorkspaceExample >> shoutAboutToStyle: ATextView [
	^ true
]

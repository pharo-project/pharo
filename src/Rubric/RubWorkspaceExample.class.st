"
self open
"
Class {
	#name : #RubWorkspaceExample,
	#superclass : #Object,
	#instVars : [
		'textModel',
		'announcer',
		'bindings',
		'mustDeclareVariables'
	],
	#category : 'Rubric-Editing-Widgets-Examples'
}

{ #category : #examples }
RubWorkspaceExample class >> exampleWindowWithScrolledText [

	^self open
]

{ #category : #examples }
RubWorkspaceExample class >> exampleWithCustomShortcut [

	 |  window text |
    text := RubScrolledTextMorph new.

    "define a custom shortcut  - notice  the call to textArea"

    text textArea removeKeyCombination: $s meta.
	 text setText: 'Hit cmd $s to revert the text'.
    text textArea on: $s meta do:[
        text setText: text text asString reverse].

    window := StandardWindow new.
    window addMorph: text fullFrame: (0@0 corner: 1@1) asLayoutFrame.
    window title: 'Custom shortcuts example'.
    window openInWorld.
]

{ #category : #'instance creation' }
RubWorkspaceExample class >> open [
	<script>
	(self windowWithScrolledText: (self new newScrolledText)) openInWorld

]

{ #category : #'private - accessing' }
RubWorkspaceExample class >> spyAnnouncer [
	"Set it to true if you want to observe textsurface announcements"
	^ false
]

{ #category : #'private - accessing' }
RubWorkspaceExample class >> title [
	^ 'Rubric workspace example'
]

{ #category : #'private - utilities' }
RubWorkspaceExample class >> windowWithScrolledText: aScrolledText [ 
	| w |
	w := StandardWindow new.
	w title: self title.
	self spyAnnouncer ifTrue: [aScrolledText textArea announcer open].
	w addMorph: aScrolledText fullFrame: (0@0 corner: 1@1) asLayoutFrame.
	^w
]

{ #category : #accessing }
RubWorkspaceExample >> acceptString: aString [
	textModel setText: aString asText.
	 ^ true
]

{ #category : #accessing }
RubWorkspaceExample >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #shout }
RubWorkspaceExample >> bindingOf: aString [ 
	
	mustDeclareVariables ifTrue: [^ nil].
	"I want to have workspace that force the user to declare  
	variables. Still subclasses may want to do something else"
	bindings isNil
		ifTrue: [self initializeBindings].
	(bindings includesKey: aString)
		ifFalse: [bindings at: aString put: nil].
	^ bindings associationAt: aString
]

{ #category : #'code pane menu' }
RubWorkspaceExample >> codePaneMenu: aMenu shifted: shifted [ 
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"
	"Not tested"
]

{ #category : #'user interface' }
RubWorkspaceExample >> getText [
	^ textModel getText
]

{ #category : #shout }
RubWorkspaceExample >> guessTypeForName: aString [
	| binding |
	
	bindings isNil ifFalse: [ 
		binding := bindings 
			at: aString
			ifAbsent: [ nil ].
		binding isNil
			ifFalse: [ ^ binding class ] ].

	^ nil
]

{ #category : #shout }
RubWorkspaceExample >> hasBindingOf: aString [
	^(self guessTypeForName: aString) notNil
]

{ #category : #shout }
RubWorkspaceExample >> hasBindingThatBeginsWith: aString [
	^false
]

{ #category : #shout }
RubWorkspaceExample >> initialize [
	super initialize.
	textModel := RubScrolledTextModel new
		interactionModel: self;
		yourself.
	mustDeclareVariables := false
]

{ #category : #completion }
RubWorkspaceExample >> isCodeCompletionAllowed [

	^ true.
]

{ #category : #'user interface' }
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

{ #category : #'user interface' }
RubWorkspaceExample >> setText: aText from: aView [ 
	^ self acceptString: aText string
]

{ #category : #shout }
RubWorkspaceExample >> shoutAboutToStyle: ATextView [
	^ true
]

"
I provide the capability to open a floating editor to edit something on the fly.
The user is responsible for deciding on how to open the editor. See #exampleEditableStringMorph class side as an example on how to do it with #on:send:to.

One opened:
- if the escape character is pressed or if the keyboard focus is lost or in case of a mouse down anywhere else than inside the editor then the editing is aborted.
- the Enter key or Cmd+s save the contents in the Morph, then an announcement of class RubMorphEdited is sent so that the user can take desired action (checking the input, definitively accept it or reject it).

see #exampleEditableStringMorph class side 

Internal Representation and Key Implementation Points.

Instance Variables
	announcer:		<Announcer>
	editor:		<RubScrolledTextMorph>
	acceptOnCR:	<Boolean>
	initialContents: <String>
	font: <LogicalFont>
	customizeValuable: <Valuable>

Implementation notes

"
Class {
	#name : #RubFloatingEditorBuilder,
	#superclass : #Object,
	#instVars : [
		'editor',
		'announcer',
		'acceptOnCR',
		'autoAccept',
		'initialContents',
		'font',
		'customizeValuable',
		'escapeBlock'
	],
	#category : #'Rubric-Editing-Core-Support'
}

{ #category : #example }
RubFloatingEditorBuilder class >> exampleCommandLauncher [
	<sampleInstance>
	<example>
 	^RubFloatingEditorBuilder new
		acceptOnCR: false;
		customizeEditorWith: [ :editor | 
			editor
				width: 300;
				height: 50;
				center: self currentWorld center ];
		withEditedContentsDo: [ :newContents | self inform: newContents ];
		openEditorWithContents: 'self inspect'
]

{ #category : #example }
RubFloatingEditorBuilder class >> exampleEditableStringMorph [
	<example>
	| pane stringMorph edBuilder |
	pane := Morph new.
	pane
		color: Color white;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderWidth: 5.
	stringMorph := StringMorph
		contents: 'Open the mini editor: [Shift+mouseDown on the string]; Update: [Enter|Cmd+s]; Abort: [Escape]'.
	stringMorph 
		position: 5 asPoint.
		pane addMorph: stringMorph.
	edBuilder := RubFloatingEditorBuilder new.
	edBuilder
		customizeEditorWith: [ :editor | 
			editor	 
				color: Color veryLightGray veryMuchLighter;
				bounds:
					(((stringMorph bounds withRight: pane right) withBottom: stringMorph bottom + 40)
						translateBy: 2 negated asPoint) ];
		withEditedContentsDo:
				[ :editedContents | 
			| newContents |
			newContents := editedContents ifEmpty: [ 'Ahem! I need something here...' ].
			stringMorph contents: newContents.
			self inform: newContents ].
	stringMorph
		on: #mouseDown
		send: #value:
		to: [ :event | 
			event shiftPressed
				ifTrue: [ edBuilder openEditorWithContents: stringMorph contents ] ].
	(pane embeddedInMorphicWindowLabeled: 'Editable sm example')
		openInWorldExtent: (stringMorph extent translateBy:  30 @ 30)
]

{ #category : #accessing }
RubFloatingEditorBuilder >> acceptOnCR: aBoolean [
	acceptOnCR := aBoolean
]

{ #category : #accessing }
RubFloatingEditorBuilder >> announcer [
	^ announcer
]

{ #category : #accessing }
RubFloatingEditorBuilder >> announcerDo: aBlock [
	announcer ifNil: [ ^ self ].
	aBlock cull: announcer
]

{ #category : #accessing }
RubFloatingEditorBuilder >> autoAccept: aBoolean [
	autoAccept := aBoolean
]

{ #category : #private }
RubFloatingEditorBuilder >> buildEditor [
	editor := RubScrolledTextMorph new forbidMenu.
	editor
		autoAccept: autoAccept;
		setTextWith: self initialContents.
	self currentWorld activeHand addEventListener: self.
	editor announcer
		when: MorphLostFocus send: #whenEditorLostFocus: to: self;
		when: RubTextAccepted send: #whenTextAccepted: to: self;
		when: RubReturnEntered send: #whenReturnEntered: to: self;
		when: RubKeystroke send: #whenKeystroke: to: self;
		when: MorphDeleted send: #whenEditorDeleted: to: self.
	editor
		beWrapped;
		font: font;
		margins: (Margin left: 2 right: 2 top: 0 bottom: 1);
		selectFrom: initialContents size + 1 to: initialContents size.

	"until we have better a rub text editor for text field like components"
	editor textArea on: PharoShortcuts current findShortcut do:[:x | "nothing, we don't need a search within a floating editor widget"].
	editor textArea withoutSelectionBar.
	^ self customizedEditor
]

{ #category : #'announcement handling' }
RubFloatingEditorBuilder >> closeEditor [
	editor ifNil: [ ^ self ].
	editor delete
]

{ #category : #accessing }
RubFloatingEditorBuilder >> customizeEditorWith: aBlock [
	customizeValuable := aBlock
]

{ #category : #private }
RubFloatingEditorBuilder >> customizedEditor [
	customizeValuable cull: editor.
	^ editor
]

{ #category : #initialization }
RubFloatingEditorBuilder >> defaultAnnouncer [
	^ Announcer new
]

{ #category : #accessing }
RubFloatingEditorBuilder >> editorDo: aBlock [
	editor ifNil: [ ^ self ].
	aBlock cull: editor
]

{ #category : #accessing }
RubFloatingEditorBuilder >> font: aFont [
	font := aFont
]

{ #category : #'events-processing' }
RubFloatingEditorBuilder >> handleListenEvent: anEvent [
	anEvent isMouse
		ifFalse: [ ^ self ].
	anEvent isMouseDown
		ifFalse: [ ^ self ].
	(editor boundsInWorld containsPoint: anEvent position)
		ifFalse: [ self closeEditor ]
]

{ #category : #accessing }
RubFloatingEditorBuilder >> initialContents [
	^ initialContents
]

{ #category : #accessing }
RubFloatingEditorBuilder >> initialContents: anObject [
	initialContents := anObject asText
]

{ #category : #initialization }
RubFloatingEditorBuilder >> initialize [
	super initialize.
	announcer := self defaultAnnouncer.
	initialContents := ''.
	acceptOnCR := true.
	autoAccept := false.
	escapeBlock := [  ].
	font := StandardFonts defaultFont.
	customizeValuable := [  ]
]

{ #category : #'public editing' }
RubFloatingEditorBuilder >> openEditor [
	self buildEditor.
	editor ifNil: [ ^ self ].
	editor
		openInWorld;
		comeToFront.
	editor textArea takeKeyboardFocus
]

{ #category : #'public editing' }
RubFloatingEditorBuilder >> openEditorWithContents: aText [
	self
		initialContents: aText;
		openEditor
]

{ #category : #'public editing' }
RubFloatingEditorBuilder >> openEditorWithContents: aText thenDo: aBlock [
	self
		initialContents: aText;
		openEditor.
	aBlock cull: editor
]

{ #category : #'announcement handling' }
RubFloatingEditorBuilder >> whenEditorDeleted: anAnnouncement [
	anAnnouncement morph activeHand removeEventListener: self.
	anAnnouncement morph announcer unsubscribe: self.
	editor := nil
]

{ #category : #accessing }
RubFloatingEditorBuilder >> whenEditorEscapedDo: aBlock [
	escapeBlock := aBlock
]

{ #category : #'announcement handling' }
RubFloatingEditorBuilder >> whenEditorLostFocus: anAnnouncement [
	self closeEditor
]

{ #category : #'announcement handling' }
RubFloatingEditorBuilder >> whenKeystroke: anAnnouncement [
	anAnnouncement event keyCharacter = Character escape
		ifTrue: [ escapeBlock value.
			self closeEditor ]
]

{ #category : #'announcement handling' }
RubFloatingEditorBuilder >> whenReturnEntered: anAnnouncement [
	acceptOnCR
		ifTrue: [ anAnnouncement accepted: true ].
	autoAccept
		ifTrue:
			[ | ann |
			ann := RubMorphEdited morph: editor.
			ann previousContents: initialContents.
			self announcer announce: ann yourself.
			self closeEditor ]
]

{ #category : #'announcement handling' }
RubFloatingEditorBuilder >> whenTextAccepted: anAnnouncement [
	self
		editorDo:
			[ :ed | 
			| ann |
			autoAccept
				ifFalse: [ self closeEditor ].
			ann := RubMorphEdited morph: ed.
			ann previousContents: initialContents.
			self announcer announce: ann ]
]

{ #category : #accessing }
RubFloatingEditorBuilder >> withEditedContentsDo: aBlock [
	self
		announcerDo:
			[ :anncer | 
			anncer
				when: RubMorphEdited
				send: #value:
				to: [ :ann  | aBlock cull: ann morph text cull: ann morph ] ]
]

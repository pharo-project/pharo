"
I represent the mode to use for editing smalltalk code
"
Class {
	#name : 'RubSmalltalkCodeMode',
	#superclass : 'RubAbstractSmalltalkMode',
	#instVars : [
		'classOrMetaclass'
	],
	#category : 'Rubric-Editing-Code',
	#package : 'Rubric',
	#tag : 'Editing-Code'
}

{ #category : 'menu' }
RubSmalltalkCodeMode class >> editingModeMenuOn: aBuilder [
	"Specify the menu used to select an editing mode"

	<contextMenu>
	<RubEditingModeMenu>
	(aBuilder item: self label)
		action: [:model | model textArea editingMode: self new]
]

{ #category : 'accessing' }
RubSmalltalkCodeMode class >> label [
	^ 'Smalltalk code'
]

{ #category : 'menu' }
RubSmalltalkCodeMode class >> menuKeyword [
	^ #RubSmalltalkCodeMenu
]

{ #category : 'menu' }
RubSmalltalkCodeMode class >> menuOn: aBuilder [
	"Specify the menu used when writing code."

	<contextMenu>
	<RubSmalltalkCodeMenu>
	(aBuilder item: #'Do it')
		order: 1;
		keyText: 'd';
		selector: #doIt;
		help: nil;
		iconName: #smallDoIt.
	(aBuilder item: #'Do & print it')
		keyText: 'p';
		selector: #printIt;
		help: nil;
		iconName: #smallPrintIt.
	(aBuilder item: #'Do & inspect it')
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		iconName: #smallInspectIt.
	(aBuilder item: #'Do & browse its class')
		keyText: 'b';
		selector: #browseFullClass;
		help: nil;
		iconName: #smallSystemBrowser.
	(aBuilder item: #'Debug it')
		keyText: 'D';
		selector: #debugIt;
		help: nil;
		iconName: #smallDebug;
		withSeparatorAfter.
	(aBuilder item: #'Find...')
		keyText: 'f';
		selector: #find;
		help: nil;
		iconName: #smallFind.
	(aBuilder item: #'Find again')
		keyText: 'g';
		selector: #findAgain;
		help: nil;
		iconName: #smallFind.
	self extendedSearchMenuOn: aBuilder.
	(aBuilder item: #'Redo')
		keyText: 'Z';
		selector: #redo;
		help: nil;
		iconName: #smallRedo.
	(aBuilder item: #Undo)
		keyText: 'z';
		selector: #undo;
		help: nil;
		iconName: #smallUndo;
		withSeparatorAfter.
	(aBuilder item: #Copy)
		keyText: 'c';
		selector: #copySelection;
		help: nil;
		iconName: #smallCopy.
	(aBuilder item: #Cut)
		keyText: 'x';
		selector: #cut;
		help: nil;
		iconName: #smallCut.
	(aBuilder item: #Paste)
		keyText: 'v';
		selector: #paste;
		help: nil;
		iconName: #smallPaste.
	(aBuilder item: #'Paste Recent')
		selector: #pasteRecent;
		help: nil;
		iconName: #smallCopy.
	(aBuilder item: #'Indent')
		keyText: 'R';
		selector: #indent;
		help: nil.
	(aBuilder item: #'Outdent')
		keyText: 'L';
		selector: #outdent;
		help: nil.
	(aBuilder item: #'Widen selection')
		keyText: '2';
		selector: #widenSelectionOfIt;
		help: nil;
		iconName: #smallFullscreen.
	(aBuilder item: #'Compare selection to clipboard')
		keyText: 'C';
		selector: #compareToClipboard;
		help: nil;
		iconName: #diff;
		withSeparatorAfter.
	aBuilder model
		ifNotNil: [ :mod |
			mod model
				ifNotNil: [ (aBuilder item: #Accept)
						keyText: 's';
						selector: #accept;
						iconName: #smallOk.
					(aBuilder item: #Cancel)
						keyText: 'l';
						selector: #cancel;
						iconName: #smallCancel ] ]
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> bindingOf: varName [
	^ self model bindingOf: varName
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> bindings [
	^ self model bindings
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> classOrMetaClass: aBehavior [
	"set the classOrMetaClass in the receiver's styler to aBehavior"
	classOrMetaclass := aBehavior.
	self updateStyler
]

{ #category : 'helper' }
RubSmalltalkCodeMode >> formatMethodCode [

	| source tree formatted |
	self editor closeTypeIn.
	self textArea hasSelection
		ifTrue: [
			source := self textArea selection.
			tree := self parseExpression: source ]
		ifFalse: [
			source := self textArea text.
			tree := self parseSource: source ].
	tree checkFaulty: [ :msg :pos |
		^ self editor notify: msg at: pos + 1 in: source ].
	formatted := tree formattedCodeIn: classOrMetaclass.
	"Text can = String, but String never = Text"
	formatted = source asString ifTrue: [ ^ self ].
	self textArea hasSelection
		ifTrue: [ self textArea replaceSelectionWith: formatted ]
		ifFalse: [ self textArea updateTextWith: formatted ]
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> hasBindingOf: aString [
	"For the shout styler"
	^ self model isNotNil and: [ self model hasBindingOf: aString ]
]

{ #category : 'parsing' }
RubSmalltalkCodeMode >> parseExpression: aString [

	^OCParser parseFaultyExpression: aString
]

{ #category : 'parsing' }
RubSmalltalkCodeMode >> parseSource: aString [

	^OCParser parseFaultyMethod: aString
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> shoutAboutToStyle: myShoutStyler [
	^ self model isNil
		or: [
			(self model interactionModel respondsTo: #shoutAboutToStyle:) not
				or: [ self model interactionModel shoutAboutToStyle: self ] ]
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> shoutStyler [
	^ self textArea shoutStyler
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> updateStyler [

	self textArea ifNil: [ ^ self ].
	self textArea shoutStyler ifNotNil: [ :s | s classOrMetaClass: classOrMetaclass ]
]

{ #category : 'update' }
RubSmalltalkCodeMode >> updateTextAreaWhenPlugged [
	self withShoutStyler.
	super updateTextAreaWhenPlugged
]

{ #category : 'update' }
RubSmalltalkCodeMode >> updateTextAreaWhenUnplugged [
	self textArea withoutShoutStyler.
	super updateTextAreaWhenUnplugged
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> withShoutStyler [
	self textArea withDecoratorNamed: #shoutStyler.
	self updateStyler
]

{ #category : 'shout' }
RubSmalltalkCodeMode >> withoutShoutStyler [
	self textArea withoutDecoratorNamed: #shoutStyler
]

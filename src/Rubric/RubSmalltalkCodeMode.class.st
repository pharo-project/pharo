"
I represent the mode to use for editing smalltalk code
"
Class {
	#name : #RubSmalltalkCodeMode,
	#superclass : #RubAbstractSmalltalkMode,
	#instVars : [
		'classOrMetaclass'
	],
	#category : #'Rubric-Editing-Code'
}

{ #category : #menu }
RubSmalltalkCodeMode class >> editingModeMenuOn: aBuilder [
	"Specify the menu used to select an editing mode"

	<contextMenu>
	<RubEditingModeMenu>
	(aBuilder item: self label translated)
		action: [:model | model textArea editingMode: self new]
]

{ #category : #accessing }
RubSmalltalkCodeMode class >> label [
	^ 'Smalltalk code'
]

{ #category : #menu }
RubSmalltalkCodeMode class >> menuKeyword [
	^ 'RubSmalltalkCodeMenu'
]

{ #category : #menu }
RubSmalltalkCodeMode class >> menuOn: aBuilder [
	"Specify the menu used when writing code."

	<contextMenu>
	<RubSmalltalkCodeMenu>
	(aBuilder item: #'Do it' translated)
		order: 1;
		keyText: 'd';
		selector: #doIt;
		help: nil;
		iconName: #smallDoItIcon.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		iconName: #smallPrintItIcon.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'Basic Inspect it' translated)
		keyText: 'I';
		selector: #basicInspectIt;
		help: nil;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'Debug it' translated)
		keyText: 'D';
		selector: #debugIt;
		help: nil;
		iconName: #smallDebugIcon.
	(aBuilder item: #'Profile it' translated)
		selector: #tallyIt;
		iconName: #smallDebugIcon;
		withSeparatorAfter.
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		help: nil;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		help: nil;
		iconName: #smallFindIcon.
	self extendedSearchMenuOn: aBuilder.
	(aBuilder item: #'Redo' translated)
		keyText: 'Z';
		selector: #redo;
		help: nil;
		iconName: #smallRedoIcon.
	(aBuilder item: #Undo translated)
		keyText: 'z';
		selector: #undo;
		help: nil;
		iconName: #smallUndoIcon;
		withSeparatorAfter.
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		help: nil;
		iconName: #smallCopyIcon.
	(aBuilder item: #Cut translated)
		keyText: 'x';
		selector: #cut;
		help: nil;
		iconName: #smallCutIcon.
	(aBuilder item: #Paste translated)
		keyText: 'v';
		selector: #paste;
		help: nil;
		iconName: #smallPasteIcon.
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		help: nil;
		iconName: #smallCopyIcon;
		withSeparatorAfter.
	aBuilder model
		ifNotNil: [ :mod | 
			mod model
				ifNotNil: [ (aBuilder item: #Accept translated)
						keyText: 's';
						selector: #accept;
						iconName: #smallOkIcon.
					(aBuilder item: #Cancel translated)
						keyText: 'l';
						selector: #cancel;
						iconName: #smallCancelIcon ] ]
]

{ #category : #shout }
RubSmalltalkCodeMode >> bindingOf: varName [
	^ self model bindingOf: varName
]

{ #category : #shout }
RubSmalltalkCodeMode >> classOrMetaClass: aBehavior [
	"set the classOrMetaClass in the receiver's styler to aBehavior"
	classOrMetaclass := aBehavior.
	self updateStyler
]

{ #category : #helper }
RubSmalltalkCodeMode >> formatMethodCode [
	| source tree formatted |
	source := self textArea text asString.
	tree := RBParser parseMethod: source onError: [ :msg :pos | ^ self ].
	formatted := tree formattedCode.
	formatted = source
		ifTrue: [ ^ self ].
	self textArea updateTextWith: formatted
]

{ #category : #shout }
RubSmalltalkCodeMode >> hasBindingOf: aString [
	" For the shout styler "
	^ self model notNil and: [ self model hasBindingOf: aString ]
]

{ #category : #shout }
RubSmalltalkCodeMode >> hasBindingThatBeginsWith: aString [ 
	" For the shout styler "
	^ self model notNil and: [ self model hasBindingThatBeginsWith: aString ]
]

{ #category : #parsing }
RubSmalltalkCodeMode >> parseExpression: aString [

	^RBParser parseFaultyExpression: aString
]

{ #category : #parsing }
RubSmalltalkCodeMode >> parseSource: aString [

	^RBParser parseFaultyMethod: aString
]

{ #category : #shout }
RubSmalltalkCodeMode >> shoutAboutToStyle: myShoutStyler [
	^ self model isNil
		or: [ 
			(self model interactionModel respondsTo: #shoutAboutToStyle:) not
				or: [ self model interactionModel shoutAboutToStyle: self ] ] 
]

{ #category : #shout }
RubSmalltalkCodeMode >> shoutStyler [
	^ self textArea shoutStyler
]

{ #category : #shout }
RubSmalltalkCodeMode >> updateStyler [

	self textArea ifNil: [ ^ self ].
	self textArea shoutStyler ifNotNil: [ :s | s classOrMetaClass: classOrMetaclass ]
]

{ #category : #update }
RubSmalltalkCodeMode >> updateTextAreaWhenPlugged [
	self withShoutStyler.
	super updateTextAreaWhenPlugged


]

{ #category : #update }
RubSmalltalkCodeMode >> updateTextAreaWhenUnplugged [
	self textArea withoutShoutStyler.
	super updateTextAreaWhenUnplugged


]

{ #category : #shout }
RubSmalltalkCodeMode >> withShoutStyler [
	self textArea withDecoratorNamed: #shoutStyler.
	self updateStyler.
]

{ #category : #shout }
RubSmalltalkCodeMode >> withoutShoutStyler [
	self textArea withoutDecoratorNamed: #shoutStyler
]

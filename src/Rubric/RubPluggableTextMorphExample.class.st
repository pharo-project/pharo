"
shows how to use a RubPluggableTextMorph.
see example class side
"
Class {
	#name : #RubPluggableTextMorphExample,
	#superclass : #Model,
	#traits : 'TViewModel',
	#classTraits : 'TViewModel classTrait',
	#instVars : [
		'selection',
		'scrollValue',
		'classIsMeta',
		'selectedClassName',
		'selectedMethodName',
		'enabled'
	],
	#category : #'Rubric-Editing-Widgets-Examples'
}

{ #category : #example }
RubPluggableTextMorphExample class >> example [
	<example>
	RubPluggableTextMorphExample new open
]

{ #category : #'source code area' }
RubPluggableTextMorphExample >> accept: source notifying: aController [
	| p result |
	self selectedClass ifNil: [ ^self ].
	p := self selectedClass organization protocolOrganizer protocolsOfSelector: self selector.
	p := p ifEmpty: [ Protocol unclassified ] ifNotEmpty: [p anyOne name].
	result := self selectedClass compile: source classified: p notifying: aController.
	result ifNotNil: [self changed: #clearUserEdits ].
	^ result
]

{ #category : #edits }
RubPluggableTextMorphExample >> askBeforeDiscardingEdits [
	^ false
]

{ #category : #accessing }
RubPluggableTextMorphExample >> autoAccept [
	"Answer whether the editor accepts its contents on each change."

	^ false
]

{ #category : #accessing }
RubPluggableTextMorphExample >> behavior [
	^ self selectedClass 
]

{ #category : #'events handling' }
RubPluggableTextMorphExample >> checkClassName [
	| conf |
	conf := RubConfigurationChange new.
	self selectedClass
		ifNil: [ conf configurationBlock: [ :textField | textField textColor: Color red ] ]
		ifNotNil: [ conf configurationBlock: [ :textField | textField textColor: Color black ] ].
	self selectedClassName announce: conf
]

{ #category : #'events handling' }
RubPluggableTextMorphExample >> checkMethodName [
	| conf |
	conf := RubConfigurationChange new.
	self selectedClass
		ifNil: [ conf configurationBlock: [ :textField | textField textColor: Color red ] ]
		ifNotNil: [ 
			conf
				configurationBlock: [ :textField | 
					(self selectedClass canUnderstand: self selectedMethodName getString asSymbol)
						ifFalse: [ textField textColor: Color red ]
						ifTrue: [ textField textColor: Color black ] ] ].
	self selectedMethodName announce: conf
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> classIsMeta [
	^ classIsMeta ifNil: [ classIsMeta := false ]
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> classIsMeta: aBoolean [
	classIsMeta := aBoolean.
	self changed: #classIsMeta

]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> classSelectorMorph [
	^ self selectedClassName newTextField
		withGhostText: 'Class name';
		autoAccept: true;
		hResizing: #spaceFill;
		yourself
]

{ #category : #menu }
RubPluggableTextMorphExample >> codePaneMenu: aMenu shifted: shifted [
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"
	"You should not use this way of getting menu. Because when you create a text area, you have a menu"

	| donorMenu |
	donorMenu := (PragmaMenuBuilder pragmaKeyword: RubSmalltalkCodeMode menuKeyword model: self) menu.
	^ aMenu addAllFrom: donorMenu
]

{ #category : #accessing }
RubPluggableTextMorphExample >> dragEnabled [
	^ false
]

{ #category : #accessing }
RubPluggableTextMorphExample >> dropEnabled [
	^ false
]

{ #category : #accessing }
RubPluggableTextMorphExample >> dropEnabled: aBoolean [
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> editButtonMorph [
	^ UIManager default
		newButtonFor: self
		action: #editSourceCode
		getEnabled: nil
		label: 'Edit'
		help: nil
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> editSourceCode [
	self changed: #getText
]

{ #category : #accessing }
RubPluggableTextMorphExample >> enabled [
	^ enabled ifNil: [ enabled := true ]
]

{ #category : #accessing }
RubPluggableTextMorphExample >> enabled: aBoolean [
	enabled = aBoolean ifTrue: [ ^self ].
	enabled := aBoolean.
	self changed: #enabled
]

{ #category : #accessing }
RubPluggableTextMorphExample >> getText [
	^ self method ifNil: [''] ifNotNil: [:m | m sourceCode]
]

{ #category : #accessing }
RubPluggableTextMorphExample >> help [
	^ nil
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> metaSwitchMorph [
	^ UIManager default
		newCheckboxFor: self
		getSelected: #classIsMeta
		setSelected: #classIsMeta:
		label: 'Class'
		help: 'Class side is used if checked' 
]

{ #category : #accessing }
RubPluggableTextMorphExample >> method [
	^ [ self selectedClass >> (self selectedMethodName getString ifEmpty: [ ^nil ]) asSymbol ] on: Error do: [  ]
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> methodSelectorMorph [
	^ self selectedMethodName newTextField
		withGhostText: 'Method name';
		autoAccept: true;
		hResizing: #spaceFill;
		yourself
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> newScrolledTextMorph [
	| st |
	st := RubPluggableTextMorph new
		on: self
			text: #getText
			accept: #accept:notifying:
			readSelection: #readSelection
			menu: nil
			setSelection: #setSelection:;
		getEnabledSelector: #enabled;
		askBeforeDiscardingEdits: self askBeforeDiscardingEdits;
		autoAccept: self autoAccept;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		registerScrollChanges: #scrollValueChanged:;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself.
	st
		beWrapped;
		autoAccept: false;
		beForSmalltalkCode;
		withCodeSizeFeedback.
	^ st
]

{ #category : #edits }
RubPluggableTextMorphExample >> okToChange [
	self canDiscardEdits
		ifTrue: [ ^ true ].
	^ self promptForCancel 
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> open [
	| window editor clsMorph mthMorph |
	window := (StandardWindow labelled: 'Method editor with shout') model: self.
	editor := self newScrolledTextMorph.
	clsMorph := self classSelectorMorph.
	clsMorph announcer when: RubTextChanged send: #whenClassNameChanged: to: self.
	mthMorph := self methodSelectorMorph.
	mthMorph announcer when: RubTextChanged send: #whenMethodNameChanged: to: self.
	editor
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	window
		addMorph:
			(window
				newColumn:
					{(window
						newRow:
							{(clsMorph).
							(self metaSwitchMorph).
							(mthMorph).
							(self editButtonMorph )}).
					editor})
		fullFrame: LayoutFrame identity.
	^ window openInWorld
]

{ #category : #edits }
RubPluggableTextMorphExample >> promptForCancel [
	"Ask for the appropriate action to take when unaccepted contents
	would be overwritten."

	| choice |
	choice := UIManager default
		confirm: 'Contents has been modified.\What do you want to do?' translated withCRs
		trueChoice: 'Accept' translated
		falseChoice: 'Discard' translated
		cancelChoice: 'Cancel' translated
		default: nil.
	choice
		ifNotNil: [ 
			choice
				ifTrue: [ 
					self changed: #getText.
					self changed: #clearUserEdits ]
				ifFalse: [ self changed: #clearUserEdits ] ].
	^ self canDiscardEdits
]

{ #category : #accessing }
RubPluggableTextMorphExample >> readSelection [
	^ selection
]

{ #category : #accessing }
RubPluggableTextMorphExample >> scrollValue: aPoint [
	scrollValue := aPoint
]

{ #category : #accessing }
RubPluggableTextMorphExample >> scrollValueChanged: anAnnouncement [ 
	scrollValue := anAnnouncement step 
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> selectedClass [
	| cls |
	cls := (Smalltalk globals at: (self selectedClassName getString ifEmpty: [ ^ nil ]) asSymbol ifAbsent: [ ^ nil ])
		instanceSide.
	^ self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls ]
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> selectedClassName [
	^ selectedClassName ifNil: [ selectedClassName := RubScrolledTextModel new ]
]

{ #category : #'view creation' }
RubPluggableTextMorphExample >> selectedMethodName [
	^ selectedMethodName ifNil: [ selectedMethodName := RubScrolledTextModel new ]
]

{ #category : #accessing }
RubPluggableTextMorphExample >> selector [
	^ self selectedMethodName getString asSymbol 
]

{ #category : #accessing }
RubPluggableTextMorphExample >> setSelection: anInterval [
	selection := anInterval.
	self changed: #readSelection
]

{ #category : #shout }
RubPluggableTextMorphExample >> shoutAboutToStyle: ARubricMode [
	ARubricMode classOrMetaClass: self selectedClass.
	^ true
]

{ #category : #'events handling' }
RubPluggableTextMorphExample >> whenClassNameChanged: anAnnouncement [
	self checkMethodName.
	self checkClassName 
]

{ #category : #'events handling' }
RubPluggableTextMorphExample >> whenMethodNameChanged: anAnnouncement [
	self checkMethodName
]

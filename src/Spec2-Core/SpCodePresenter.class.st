"
I'm a code (Pharo code) presenter. 
I will style and provide completion according to the behaviour being edited.
"
Class {
	#name : #SpCodePresenter,
	#superclass : #SpTextPresenter,
	#instVars : [
		'#doItContext => SpObservableSlot',
		'#doItReceiver => SpObservableSlot',
		'#behavior => SpObservableSlot',
		'#syntaxHighlight => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Text'
}

{ #category : #specs }
SpCodePresenter class >> adapterName [

	^ #CodeAdapter
]

{ #category : #commands }
SpCodePresenter class >> buildCommandsGroupWith: presenter forRoot: aCmCommandsGroup [
	aCmCommandsGroup beDisplayedAsGroup.
	aCmCommandsGroup
		register: (self codeSelectionActionCommandsGroupWith: presenter).
	aCmCommandsGroup
		register: (self codeNavigationCommandsGroupWith: presenter).
	aCmCommandsGroup register: (self codeEditionCommandsGroupWith: presenter).
	aCmCommandsGroup register: (self codeAcceptCommandsGroupWith: presenter)
]

{ #category : #commands }
SpCodePresenter class >> codeAcceptCommandsGroupWith: presenter [
	| group |
	group := (CmCommandGroup named: 'Accept commands') asSpecGroup.
	group
		description: 'Accept or cancel changes.';
		beDisplayedAsGroup.
	group register: (SpAcceptChangesCommand forSpecContext: presenter).
	group register: (SpCancelChangesCommand forSpecContext: presenter).
	^ group
]

{ #category : #commands }
SpCodePresenter class >> codeEditionCommandsGroupWith: presenter [
	| group |
	group := (CmCommandGroup named: 'Selection edition commands') asSpecGroup.
	group	description: 'Edit code selection.';
		beDisplayedAsGroup.
	SpCodeEditionCommand
		allSubclassesDo: [ :cmdClass | group register: (cmdClass forSpecContext: presenter) ].
	^ group
]

{ #category : #commands }
SpCodePresenter class >> codeNavigationCommandsGroupWith: presenter [
	| group |
	group := (CmCommandGroup named: 'Selection search commands') asSpecGroup.
	group
		description: 'Search the system from code selection.';
		beDisplayedAsGroup.
	group register: (SpBrowseClassCommand forSpecContext: [presenter findClassFromSelection]).	
	{ 
		SpBrowseImplementorsCommand.
		SpBrowseSendersCommand.
		SpBrowseMethodReferencesCommand.	
		SpBrowseMethodsContainingStringCommand.
	 } 
		do:[:navCmdClass|	group register: (navCmdClass forSpecContext: [presenter selectedText])].
	^ group
]

{ #category : #commands }
SpCodePresenter class >> codeSelectionActionCommandsGroupWith: presenter [
	| group |
	group := (CmCommandGroup named: 'Selection action commands')
		asSpecGroup.
	group
		description: 'Actions on code selection.';
		beDisplayedAsGroup.
	SpCodeSelectionCommand allSubclasses
		do: [ :cmdClass | group register: (cmdClass forSpecContext: presenter) ].
	^ group
]

{ #category : #examples }
SpCodePresenter class >> example [
	<sampleInstance>

	^ self new
	 openWithSpec;
		text:
			'some method: 42'

]

{ #category : #specs }
SpCodePresenter class >> title [

	^ 'Code'
]

{ #category : #'api-shout' }
SpCodePresenter >> behavior [
	"Return the class corresponding to the method class of the source code you are editing"
	
	^ behavior
]

{ #category : #'api-shout' }
SpCodePresenter >> behavior: aClass [
	"Set the class corresponding to the method class of the source code you are editing"

	behavior := aClass
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItContext [

	^ doItContext
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItContext: anObject [

	doItContext := anObject
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItReceiver [
	| receiver |
	
	receiver := doItReceiver.
	receiver isBehavior ifTrue: [ 
		receiver := receiver instanceSide ].
	^ receiver
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItReceiver: anObject [

	doItReceiver := anObject
]

{ #category : #navigation }
SpCodePresenter >> findClassFrom: aString [
	| ast |
	ast := RBParser parseExpression: aString onError: [ ^ nil ].
	ast
		nodesDo: [ :node | 
			(node isVariable and: [ node name first isUppercase ])
				ifTrue: [ (self class environment classNamed: node name)
						ifNotNil: [ :aClass | ^ aClass ] ] ].
	^ nil
]

{ #category : #navigation }
SpCodePresenter >> findClassFromSelection [
	^ self findClassFrom: self selectedText trimmed
]

{ #category : #testing }
SpCodePresenter >> hasSyntaxHighlight [

	^ syntaxHighlight
]

{ #category : #initialization }
SpCodePresenter >> initialize [ 

	super initialize.
	
	self withSyntaxHighlight.
	
	self registerEventsForStyling.
	
	self contextMenu: [self rootCommandsGroup asMenuPresenter].
	self contextKeyBindings: self rootCommandsGroup asKMCategory
]

{ #category : #initialization }
SpCodePresenter >> registerEventsForStyling [

	self whenBehaviorChangedDo: [ :class | 
		self changed: #classOrMetaClass: with: { class }.
		self changed: #getText ]
]

{ #category : #api }
SpCodePresenter >> selectedBehavior [

	self flag: #TODO. "What this is need for?"
	^ self behavior
]

{ #category : #navigation }
SpCodePresenter >> selectionOrLine [
	^ self selectedText
		ifEmpty: [ self
				selectLine;
				selectedText ]
]

{ #category : #accessing }
SpCodePresenter >> syntaxHighlight: aBoolean [
	syntaxHighlight := aBoolean 
]

{ #category : #'api-events' }
SpCodePresenter >> whenBehaviorChangedDo: aBlock [
	"Set a block to perform when the behavior class changed"
	
	self 
		property: #behavior 
		whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpCodePresenter >> whenSyntaxHighlightChangedDo: aBlock [
	"Set a block to perform when the syntax highlight is enabled/disabled"

	self property: #syntaxHighlight whenChangedDo: aBlock
]

{ #category : #api }
SpCodePresenter >> withSyntaxHighlight [
	self syntaxHighlight: true
]

{ #category : #api }
SpCodePresenter >> withoutSyntaxHighlight [
	self syntaxHighlight: false
]

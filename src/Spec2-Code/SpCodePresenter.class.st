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
		'#syntaxHighlight => SpObservableSlot',
		'#completionEngine => SpObservableSlot',
		'#lineNumbers => SpObservableSlot',
		'#interactionModel',
		'#overrideContextMenu'
	],
	#category : #'Spec2-Code'
}

{ #category : #specs }
SpCodePresenter class >> adapterName [

	^ #CodeAdapter
]

{ #category : #commands }
SpCodePresenter class >> buildCommandsGroupWith: presenter forRoot: aCmCommandsGroup [

	aCmCommandsGroup beDisplayedAsGroup.
	aCmCommandsGroup register: (self codeSelectionActionCommandsGroupWith: presenter).
	aCmCommandsGroup register: (self codeNavigationCommandsGroupWith: presenter)
]

{ #category : #commands }
SpCodePresenter class >> codeNavigationCommandsGroupWith: aPresenter [
	| group |
	
	group := (CmCommandGroup named: 'Selection search commands') asSpecGroup.
	group
		description: 'Search the system from code selection.';
		beDisplayedAsGroup.
	group register: (SpBrowseClassCommand new 
		transform: [ :aContext | aContext findClassFromSelection ];
		asSpecCommand).	

	self flag: #TODO. "This looks like a very bad way to do this :P"	
	{ 
		SpBrowseImplementorsCommand.
		SpBrowseSendersCommand.
		SpBrowseMethodReferencesCommand.	
		SpBrowseMethodsContainingStringCommand.
	 } 
	do:[ :commandClass | 
		group register: (commandClass new 
			transform: [ :aContext | aContext selectedTextOrLine ];
			asSpecCommand) ].
	
	^ group
]

{ #category : #commands }
SpCodePresenter class >> codeSelectionActionCommandsGroupWith: presenter [
	| group |
	
	group := (CmCommandGroup named: 'Selection action commands') asSpecGroup.
	group
		description: 'Actions on code selection.';
		beDisplayedAsGroup.
	
	SpCodeSelectionCommand allSubclassesDo: [ :cmdClass | 
		group register: (cmdClass forSpecContext: presenter) ].
	
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

{ #category : #binding }
SpCodePresenter >> bindingOf: aString [

	interactionModel ifNotNil:  [ :im | ^ im bindingOf: aString  ].
	self behavior ifNotNil: [ :aBehavior | ^ aBehavior bindingOf: aString ].
	^ nil
]

{ #category : #private }
SpCodePresenter >> buildContextKeyBindingsWith: aCategory [
	| category |
	
	category := self newContextMenuKeyBindings.
	aCategory ifNotNil: [ 
		aCategory allEntries keymaps do: [ :each |
			category addKeymapEntry: each ] ].
	
	^ category
]

{ #category : #private }
SpCodePresenter >> buildContextMenuWith: aValuable [
	| menuPresenter |
	
	overrideContextMenu ifTrue: [ ^ aValuable ].

	menuPresenter := self newContextMenu.
	aValuable ifNotNil: [ 
		aValuable value ifNotNil: [ :menu |
			menu menuGroups do: [ :aGroup |
				menuPresenter addMenuGroup: aGroup ] ] ].

	^ menuPresenter
]

{ #category : #accessing }
SpCodePresenter >> completionEngine [

	^ completionEngine
]

{ #category : #accessing }
SpCodePresenter >> completionEngine: aCompletionEngine [

	completionEngine := aCompletionEngine
]

{ #category : #'api-shortcuts' }
SpCodePresenter >> contextKeyBindings [

	^ self buildContextKeyBindingsWith: super contextKeyBindings
]

{ #category : #accessing }
SpCodePresenter >> contextMenu [
	"The code presenter context menu is a compound of two menus: 
		1) the basic interoperation menu (that includes operations like cut&paste but also do it, 
		print it, etc.
		2) the user defined context menu.
    If you want to override completely this menu you need to set the 'overrideMenu' property 
	 to true"

	^ [ self buildContextMenuWith: super contextMenu ]
]

{ #category : #binding }
SpCodePresenter >> doItContext [

	doItContext ifNotNil: [ ^ doItContext ].
	^ interactionModel ifNotNil: [ :im | im doItContext ]
]

{ #category : #binding }
SpCodePresenter >> doItContext: anObject [

	doItContext := anObject
]

{ #category : #binding }
SpCodePresenter >> doItReceiver [
	
	doItReceiver ifNotNil: [ 
		| receiver |
		receiver := doItReceiver.
		receiver isBehavior ifTrue: [ 
			receiver := receiver instanceSide ].
		^ receiver ].
	
	^ interactionModel ifNotNil: [ :im | im doItReceiver ]
]

{ #category : #binding }
SpCodePresenter >> doItReceiver: anObject [

	doItReceiver := anObject
]

{ #category : #accessing }
SpCodePresenter >> findClassFrom: aString [
	| ast |
	
	self flag: #TODO. "Does this really has to be part of the code api?"

	ast := RBParser parseExpression: aString onError: [ ^ nil ].
	ast nodesDo: [ :node | 
		(node isVariable and: [ node name first isUppercase ])
			ifTrue: [ 
				(self class environment classNamed: node name)
					ifNotNil: [ :aClass | ^ aClass ] ] ].

	^ nil
]

{ #category : #accessing }
SpCodePresenter >> findClassFromSelection [
	self flag: #TODO. "Does this really has to be part of the text api?"

	^ self findClassFrom: self selectedTextOrLine trimmed
]

{ #category : #binding }
SpCodePresenter >> hasBindingOf: aString [

	interactionModel ifNotNil: [ :im | ^ im hasBindingOf: aString ].
	self behavior ifNotNil: [ :aBehavior | (aBehavior bindingOf: aString) notNil ].
	^ false
]

{ #category : #binding }
SpCodePresenter >> hasBindingThatBeginsWith: aString [ 
	
	^ interactionModel
		ifNotNil: [ :im | im hasBindingThatBeginsWith: aString ] 
		ifNil: [ false ]
]

{ #category : #testing }
SpCodePresenter >> hasLineNumbers [

	^ lineNumbers
]

{ #category : #testing }
SpCodePresenter >> hasSyntaxHighlight [

	^ syntaxHighlight
]

{ #category : #initialization }
SpCodePresenter >> initialize [ 

	super initialize.

	"Use the default menu"
	overrideContextMenu := false.
	
	self withSyntaxHighlight.
	self withLineNumbers.
	
	self registerEventsForStyling
]

{ #category : #api }
SpCodePresenter >> insertPopoverAfterCurrentSelection: aPresenter [

	self withAdapterDo: [ :anAdapter |
		anAdapter insertPopoverAfterCurrentSelection: aPresenter ]
]

{ #category : #accessing }
SpCodePresenter >> insertPrintPopoverAfterCurrentSelection: anObject [
	"Inserts a print-it result popover. 
	 This is for use of SpCodePrintItCommand"
	
	self insertPopoverAfterCurrentSelection: (SpCodePopoverPrintPresenter 
		newCode: self
		object: anObject)
]

{ #category : #api }
SpCodePresenter >> interactionModel [

	^ interactionModel
]

{ #category : #api }
SpCodePresenter >> interactionModel: anObject [ 	

	interactionModel := anObject
]

{ #category : #accessing }
SpCodePresenter >> lineNumbers: aBoolean [

	lineNumbers := aBoolean
]

{ #category : #private }
SpCodePresenter >> newContextMenu [
	
	^ self rootCommandsGroup asMenuPresenter
]

{ #category : #private }
SpCodePresenter >> newContextMenuKeyBindings [
	
	^ self rootCommandsGroup asKMCategory
]

{ #category : #accessing }
SpCodePresenter >> overrideContextMenu: aBoolean [
	"This property set the type of menu to use: if #overrideContextMenu is false (the default), 
	 the contextMenu will include code presenter default entries. If #overrideContextMenu is true, 
	 just what is defined in #contextMenu will be displayed"

	overrideContextMenu := aBoolean
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

{ #category : #binding }
SpCodePresenter >> selectedClassOrMetaClass [

	behavior ifNotNil: [ ^ behavior ].
	^ interactionModel ifNotNil: [ :im | im selectedClassOrMetaClass ]
]

{ #category : #api }
SpCodePresenter >> selectedTextOrLine [

	self selectedText ifNotEmpty: [ :aString | ^ aString ].
	
	^ self
		selectLine;
		selectedText
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
SpCodePresenter >> whenCompletionEngineChangedDo: aBlock [
	"Set a block to perform when the syntax highlight is enabled/disabled"
	
	self property: #completionEngine whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpCodePresenter >> whenLineNumbersChangedDo: aBlock [
	"Set a block to perform when the syntax highlight is enabled/disabled"

	self property: #lineNumbers whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpCodePresenter >> whenSyntaxHighlightChangedDo: aBlock [
	"Set a block to perform when the syntax highlight is enabled/disabled"

	self property: #syntaxHighlight whenChangedDo: aBlock
]

{ #category : #api }
SpCodePresenter >> withLineNumbers [

	self lineNumbers: true
]

{ #category : #api }
SpCodePresenter >> withSyntaxHighlight [

	self syntaxHighlight: true
]

{ #category : #api }
SpCodePresenter >> withoutLineNumbers [

	self lineNumbers: false
]

{ #category : #api }
SpCodePresenter >> withoutSyntaxHighlight [

	self syntaxHighlight: false
]

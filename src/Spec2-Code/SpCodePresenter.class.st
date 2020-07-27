"
I'm a code (Pharo code) presenter. 
I will style and provide completion according to the behaviour being edited.

@@TODO

!! Transmissions
- #inputTextPort (default)
- #outputTextChangedPort (default)
"
Class {
	#name : #SpCodePresenter,
	#superclass : #SpTextPresenter,
	#instVars : [
		'#syntaxHighlight => SpObservableSlot',
		'#lineNumbers => SpObservableSlot',
		'#interactionModel => SpObservableSlot',
		'#overrideContextMenu',
		'#submitBlock',
		'#resetBlock'
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
SpCodePresenter class >> buildEditionCommandsGroupWith: presenter forRoot: aCmCommandsGroup [

	super buildEditionCommandsGroupWith: presenter forRoot: aCmCommandsGroup.	
	aCmCommandsGroup register: (self codeAcceptCommandsGroupWith: presenter)
]

{ #category : #commands }
SpCodePresenter class >> codeAcceptCommandsGroupWith: aPresenter [
	| commandGroup |

	commandGroup := (CmCommandGroup named: 'Accept commands') asSpecGroup
		description: 'Accept or cancel changes.';
		beDisplayedAsGroup;
		yourself.
	
	aPresenter hasSubmitAction ifTrue: [ 
		commandGroup register: (SpAcceptChangesCommand forSpecContext: aPresenter) ].
	aPresenter hasResetAction ifTrue: [ 
		commandGroup register: (SpCancelChangesCommand forSpecContext: aPresenter) ].

	^ commandGroup
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

{ #category : #private }
SpCodePresenter >> basicInteractionModel: anObject [

	interactionModel := anObject
]

{ #category : #'api-model' }
SpCodePresenter >> beForBehavior: aBehavior [
	"used when browsing classes or alike"

	self interactionModel: (SpCodeBehaviorInteractionModel on: aBehavior)
]

{ #category : #'api-model' }
SpCodePresenter >> beForMethod: aMethod [
	"used when browsing methods"

	self interactionModel: (SpCodeMethodInteractionModel on: aMethod)
]

{ #category : #'api-model' }
SpCodePresenter >> beForObject: anObject [
	"Used when inspecting objects"

	self interactionModel: (SpCodeObjectInteractionModel on: anObject)
]

{ #category : #'api-model' }
SpCodePresenter >> beForScripting [
	"Use in playgrounds"

	self interactionModel: SpCodeScriptingInteractionModel new
]

{ #category : #'private bindings' }
SpCodePresenter >> behavior [
	"Return the class corresponding to the method class of the source code you are editing"
	
	^ self interactionModel ifNotNil: [ :im | im behavior ]
]

{ #category : #'private bindings' }
SpCodePresenter >> behavior: aClass [
	"Set the class corresponding to the method class of the source code you are editing"
	
]

{ #category : #'private bindings' }
SpCodePresenter >> bindingOf: aString [

	self interactionModel ifNotNil:  [ :im | ^ im bindingOf: aString  ].
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

{ #category : #api }
SpCodePresenter >> clearInteractionModel [

	self interactionModel: SpCodeNullInteractionModel new
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

{ #category : #'private bindings' }
SpCodePresenter >> doItContext [

	^ self interactionModel ifNotNil: [ :im | im doItContext ]
]

{ #category : #'private bindings' }
SpCodePresenter >> doItContext: anObject [

]

{ #category : #'private bindings' }
SpCodePresenter >> doItReceiver [
	
	^ self interactionModel ifNotNil: [ :im | im doItReceiver ]
]

{ #category : #'private bindings' }
SpCodePresenter >> doItReceiver: anObject [

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

{ #category : #'private bindings' }
SpCodePresenter >> hasBindingOf: aString [

	self interactionModel ifNotNil: [ :im | ^ im hasBindingOf: aString ].
	^ false
]

{ #category : #'private bindings' }
SpCodePresenter >> hasBindingThatBeginsWith: aString [ 
	
	^ self interactionModel
		ifNotNil: [ :im | im hasBindingThatBeginsWith: aString ] 
		ifNil: [ false ]
]

{ #category : #testing }
SpCodePresenter >> hasLineNumbers [

	^ lineNumbers
]

{ #category : #testing }
SpCodePresenter >> hasResetAction [

	^ resetBlock notNil
]

{ #category : #testing }
SpCodePresenter >> hasSubmitAction [

	^ submitBlock notNil
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
	
	self registerEventsForStyling.
	self clearInteractionModel
]

{ #category : #api }
SpCodePresenter >> insertPopover: aPresenter atIndex: location [

	self selectionInterval: ((location + 1) to: location).
	self insertPopoverAfterCurrentSelection: aPresenter
]

{ #category : #api }
SpCodePresenter >> insertPopoverAfterCurrentSelection: aPresenter [

	self withAdapterDo: [ :anAdapter |
		anAdapter insertPopoverAfterCurrentSelection: aPresenter ]
]

{ #category : #api }
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
SpCodePresenter >> interactionModel: anInteractionModel [

	anInteractionModel owner: self.
	self basicInteractionModel: anInteractionModel
]

{ #category : #'private bindings' }
SpCodePresenter >> isForScripting [

	^ self interactionModel 
		ifNotNil: [ :im | im isForScripting ]
		ifNil: [ false ]
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

	self whenInteractionModelChangedDo: [ :im |
		self withAdapterDo: [ :anAdapter |
			anAdapter interactionModel: im ] ]
]

{ #category : #api }
SpCodePresenter >> selectedBehavior [

	^ self behavior
]

{ #category : #'private bindings' }
SpCodePresenter >> selectedClassOrMetaClass [

	^ self interactionModel ifNotNil: [ :im | im selectedClassOrMetaClass ]
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
SpCodePresenter >> triggerResetAction [

	resetBlock ifNil: [ ^ self ].
	resetBlock value
]

{ #category : #'api-events' }
SpCodePresenter >> triggerSubmitAction [

	submitBlock ifNil: [ ^ self ].
	submitBlock value: self text
]

{ #category : #api }
SpCodePresenter >> userActionDuring: aBlock [
	"There are a buch of operations that will all trigger a backend update. A tipical case, 
	 changing text and interactionModel will trigger the text-changed event 3 times and that 
	 may cause an excesive compute that we want to avoid. 
	 We use #userActionDuring: to avoid that. 
	[[[language=Smalltalk
	aCodePresenter userActionDuring: [ 
		aCodePresenter
			beForScription;
			text: '42 factorial' ]
	]]]."
	
	self withAdapterDo: [ :anAdapter | 
		anAdapter userActionDuring: aBlock.
		^ self ].
	
	aBlock value
]

{ #category : #'api-events' }
SpCodePresenter >> whenInteractionModelChangedDo: aBlock [
	"Set a block to perform when the behavior class changed"
	
	self 
		property: #interactionModel
		whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpCodePresenter >> whenLineNumbersChangedDo: aBlock [
	"Set a block to perform when the syntax highlight is enabled/disabled"

	self property: #lineNumbers whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpCodePresenter >> whenResetDo: aBlock [

	resetBlock := aBlock.
	super whenResetDo: aBlock
]

{ #category : #'api-events' }
SpCodePresenter >> whenSubmitDo: aBlock [

	submitBlock := aBlock.
	super whenSubmitDo: aBlock
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

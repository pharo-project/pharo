"
I am one command declared in a Calypso command map: the command class and where and how it is shown for one interaction (a context menu, a toolbar, the tab actions, the table icons...).
I am created by the methods marked with <calypsoCommands> through a ClyCommandInteraction, for example:
	(aCommandMap contextMenu add: SycRemoveMethodCommand) order: 10000.
I hold the placement: the menu group class, the order and the label. An order or a label I do not have is asked to the command when it is shown (#orderInContext: and the label selector of my interaction).
My subclasses are the entries of the interactions which need more: the key combination of a shortcut (ClyShortcutCommandEntry), the mouse button of a click (ClyMouseCommandEntry) and the drop anchor of drag and drop (ClyDropCommandEntry), the browser tool of a tab (ClyTabEntry) and the decorator of table cells (ClyTableDecoratorEntry).
I remember the method which declared me, and the one which changed me with an override such as ClyCommandInteraction>>#rebind:to:, so tools can show why a command appears somewhere.
I answer the messages Calypso asks to command activations (#menuGroup, #menuItemNameFor:, #isActiveInContext:, #newActivatorFor: ...), so Calypso handles me and the class annotations of the deprecated Commander-Calypso-Compatibility package alike.
"
Class {
	#name : 'ClyCommandEntry',
	#superclass : 'Object',
	#instVars : [
		'interaction',
		'commandClass',
		'group',
		'order',
		'label',
		'declaringMethod',
		'priority',
		'openedGroup',
		'overridingMethod'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandEntry class >> of: aCommandClass in: aCommandInteraction group: aMenuGroupClass [

	^ self new
		  commandClass: aCommandClass;
		  interaction: aCommandInteraction;
		  group: aMenuGroupClass;
		  declaringMethod: aCommandInteraction map declaringMethod;
		  yourself
]

{ #category : 'accessing' }
ClyCommandEntry >> anchor [

	^ interaction map anchor
]

{ #category : 'activation' }
ClyCommandEntry >> annotates: aCommandClass [

	^ commandClass == aCommandClass
]

{ #category : 'accessing' }
ClyCommandEntry >> commandClass [

	^ commandClass
]

{ #category : 'accessing' }
ClyCommandEntry >> commandClass: aCommandClass [

	commandClass := aCommandClass
]

{ #category : 'accessing' }
ClyCommandEntry >> declaringMethod [
	"The method marked with <calypsoCommands> which declared me."

	^ declaringMethod
]

{ #category : 'accessing' }
ClyCommandEntry >> declaringMethod: aCompiledMethod [

	declaringMethod := aCompiledMethod
]

{ #category : 'accessing' }
ClyCommandEntry >> group [

	^ group
]

{ #category : 'accessing' }
ClyCommandEntry >> group: aMenuGroupClass [

	group := aMenuGroupClass
]

{ #category : 'accessing' }
ClyCommandEntry >> interaction [

	^ interaction
]

{ #category : 'accessing' }
ClyCommandEntry >> interaction: aCommandInteraction [

	interaction := aCommandInteraction
]

{ #category : 'activation' }
ClyCommandEntry >> isActiveInContext: aToolContext [
	"The context must be described by my anchor and my command must be able to run in it (#canBeExecuted). Menus, toolbars, tab actions and table icons also ask the command whether it shows there (ClyCommand>>#isAvailableInMenuOf:)."

	| command |
	(self isVisibleInContext: aToolContext) ifFalse: [ ^ false ].
	command := self newCommandInContext: aToolContext.
	command canBeExecuted ifFalse: [ ^ false ].
	^ interaction hidesCommandsThatCannotRun not or: [ command isAvailableInMenuOf: aToolContext ]
]

{ #category : 'activation' }
ClyCommandEntry >> isForbidden [

	^ commandClass isAbstract
]

{ #category : 'activation' }
ClyCommandEntry >> isSimilarTo: anotherEntry [

	^ self == anotherEntry
]

{ #category : 'activation' }
ClyCommandEntry >> isVisibleInContext: aToolContext [

	^ aToolContext isDescribedByCommandAnchor: self anchor
]

{ #category : 'accessing' }
ClyCommandEntry >> label [

	^ label
]

{ #category : 'accessing' }
ClyCommandEntry >> label: aString [

	label := aString
]

{ #category : 'activation' }
ClyCommandEntry >> menuGroup [

	^ group
]

{ #category : 'activation' }
ClyCommandEntry >> menuItemName [

	^ label
]

{ #category : 'activation' }
ClyCommandEntry >> menuItemNameFor: aCommand [

	^ label ifNil: [ interaction labelOf: aCommand ]
]

{ #category : 'activation' }
ClyCommandEntry >> menuItemOrder [

	^ order
]

{ #category : 'activation' }
ClyCommandEntry >> menuItemOrderFor: aCommand [

	^ order ifNil: [ aCommand orderInContext: aCommand context ]
]

{ #category : 'activation' }
ClyCommandEntry >> newActivatorFor: aToolContext [

	^ ClyCommandActivator new
		  command: (self newCommandInContext: aToolContext);
		  context: aToolContext;
		  activationStrategy: self;
		  yourself
]

{ #category : 'activation' }
ClyCommandEntry >> newCommandInContext: aToolContext [

	| command |
	command := commandClass new
		           context: aToolContext;
		           yourself.
	openedGroup ifNotNil: [ command menuGroupClass: openedGroup ].
	^ command
]

{ #category : 'accessing' }
ClyCommandEntry >> openedGroup [
	"The menu group whose commands my ClyOpenMenuGroupCommand opens, see ClyCommandInteraction>>#addMenuOf:."

	^ openedGroup
]

{ #category : 'accessing' }
ClyCommandEntry >> openedGroup: aMenuGroupClass [

	openedGroup := aMenuGroupClass
]

{ #category : 'accessing' }
ClyCommandEntry >> order [

	^ order
]

{ #category : 'accessing' }
ClyCommandEntry >> order: aNumber [

	order := aNumber
]

{ #category : 'accessing' }
ClyCommandEntry >> overridingMethod [
	"The declaration method which changed me with an override such as ClyCommandInteraction>>#rebind:to:, or nil."

	^ overridingMethod
]

{ #category : 'accessing' }
ClyCommandEntry >> overridingMethod: aCompiledMethod [

	overridingMethod := aCompiledMethod
]

{ #category : 'printing' }
ClyCommandEntry >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		print: commandClass;
		nextPutAll: ' in ';
		nextPutAll: interaction name;
		nextPutAll: ' of ';
		print: self anchor;
		nextPut: $)
]

{ #category : 'accessing' }
ClyCommandEntry >> priority [
	"When several commands share a key, a click or a drop, the first one of the highest priority runs."

	^ priority ifNil: [ 0 ]
]

{ #category : 'accessing' }
ClyCommandEntry >> priority: aNumber [

	priority := aNumber
]

"
I am one command declared in a Calypso command map: the command class and where and how it is shown for one interaction (a context menu, a toolbar, a shortcut, a click, a drop...).
I am created by the methods marked with <calypsoCommands> through a ClyCommandInteraction, for example:
	(aCommandMap contextMenu add: SycRemoveMethodCommand) order: 10000.
	aCommandMap shortcuts add: SycRemoveMethodCommand key: $x meta.
I hold the placement: the menu group class, the order, the label, the key combination, the mouse button and modifier, and the anchor where the dragged items can be dropped. An order or a label I do not have is asked to the command when it is shown (#orderInContext: and the label selector of my interaction).
I remember the method which declared me, so tools can show why a command appears somewhere.
I answer the messages Calypso asks to command activations (#menuGroup, #menuItemNameFor:, #isActiveInContext:, #newActivatorFor: ...), so the existing menu builders and renderers can show me.
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
		'keyCombination',
		'mouseButton',
		'keyboardModifier',
		'dropAnchor',
		'declaringMethod'
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
ClyCommandEntry >> dropAnchor [
	"The anchor of the contexts where the dragged items can be dropped to run my command."

	^ dropAnchor
]

{ #category : 'accessing' }
ClyCommandEntry >> dropAnchor: aClass [

	dropAnchor := aClass
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

	^ (self isVisibleInContext: aToolContext) and: [ aToolContext allowsExecutionOf: commandClass ]
]

{ #category : 'activation' }
ClyCommandEntry >> isDropActiveInContext: aToolContext [

	^ aToolContext isDescribedByCommandAnchor: dropAnchor
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
ClyCommandEntry >> keyCombination [

	^ keyCombination
]

{ #category : 'accessing' }
ClyCommandEntry >> keyCombination: aKeyCombination [

	keyCombination := aKeyCombination
]

{ #category : 'accessing' }
ClyCommandEntry >> keyboardModifier [

	^ keyboardModifier
]

{ #category : 'accessing' }
ClyCommandEntry >> keyboardModifier: aKMModifier [

	keyboardModifier := aKMModifier
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

{ #category : 'accessing' }
ClyCommandEntry >> mouseButton [
	"The mouse button of a click entry: #red (the default), #yellow or #blue."

	^ mouseButton ifNil: [ #red ]
]

{ #category : 'accessing' }
ClyCommandEntry >> mouseButton: aSymbol [

	mouseButton := aSymbol
]

{ #category : 'activation' }
ClyCommandEntry >> newActivatorFor: aToolContext [

	^ ClyCommandActivator for: commandClass inContext: aToolContext using: self
]

{ #category : 'accessing' }
ClyCommandEntry >> order [

	^ order
]

{ #category : 'accessing' }
ClyCommandEntry >> order: aNumber [

	order := aNumber
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

{ #category : 'activation' }
ClyCommandEntry >> priority [
	"Entries have no priority: when several commands share a key or a click, the first declared one runs."

	^ 0
]

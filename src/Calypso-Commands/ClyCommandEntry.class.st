"
I am one command declared in a Calypso command map: the command class and where and how it is shown for one interaction (a context menu, a toolbar, the tab actions, the table icons...).
I am created by the methods marked with <calypsoCommands> through a ClyCommandInteraction, for example:
	(aCommandMap contextMenu add: SycRemoveMethodCommand) order: 10000.
I hold the placement: the menu group class, the order and the label. An order or a label I do not have is asked to the command when it is shown (#orderInContext: and the label selector of my interaction).
My subclasses are the entries of the interactions which need more: the key combination of a shortcut (ClyShortcutCommandEntry), the mouse button of a click (ClyMouseCommandEntry) and the drop anchor of drag and drop (ClyDropCommandEntry).
I remember the method which declared me, so tools can show why a command appears somewhere.
I answer the messages Calypso asks to command activations (#menuGroup, #menuItemNameFor:, #isActiveInContext:, #newActivatorFor: ...). ClyCommandActivationStrategy class>>#registeredInstances answers me with the class annotations of my interaction, so Calypso shows both.
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
		'priority'
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

	^ (self isVisibleInContext: aToolContext) and: [ aToolContext allowsExecutionOf: commandClass ]
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
	"When several commands share a key, a click or a drop, the first one of the highest priority runs, as for class annotations."

	^ priority ifNil: [ 0 ]
]

{ #category : 'accessing' }
ClyCommandEntry >> priority: aNumber [

	priority := aNumber
]

"
I am the commands of one interaction in a Calypso command map, such as its context menu, its toolbar or its shortcuts. Declaration methods add commands to me:
	aCommandMap contextMenu add: SycRemoveMethodCommand order: 10000.
	(aCommandMap contextMenu group: SycMethodRefactoringMenuGroup) add: SycPullUpMethodCommand.
	aCommandMap shortcuts add: SycRemoveMethodCommand key: $x meta.
	aCommandMap dragAndDrop add: SycMoveMethodsToClassCommand dropIn: ClyClass.
#group: answers a view of me which adds its commands to a menu group class. The views share my entries.
#addAllConcreteSubclassesOf: adds a whole family of commands. Use it only for closed families whose hierarchy is the menu, such as view mode switches. Otherwise add each command, so a command does not have to subclass a family to be shown.
My label selector is the message asked to a command for its label when its entry has none: #name for menus, #defaultToolbarItemName for the toolbar.
"
Class {
	#name : 'ClyCommandInteraction',
	#superclass : 'Object',
	#instVars : [
		'map',
		'name',
		'entries',
		'group',
		'labelSelector',
		'entryClass'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandInteraction class >> named: aSymbol in: aCommandMap entryClass: aCommandEntryClass labelSelector: aSelector [

	^ self new
		  setName: aSymbol
		  map: aCommandMap
		  entryClass: aCommandEntryClass
		  labelSelector: aSelector
]

{ #category : 'declaring' }
ClyCommandInteraction >> add: aCommandClass [
	"Answers the new entry, to set its order, label or key."

	^ entries add: (entryClass of: aCommandClass in: self group: group)
]

{ #category : 'declaring' }
ClyCommandInteraction >> add: aCommandClass dropIn: anAnchorClass [
	"The command runs when the items it applies to are dropped on a context described by anAnchorClass."

	^ (self add: aCommandClass)
		  dropAnchor: anAnchorClass;
		  yourself
]

{ #category : 'declaring' }
ClyCommandInteraction >> add: aCommandClass key: aKeyCombination [

	^ (self add: aCommandClass)
		  keyCombination: aKeyCombination;
		  yourself
]

{ #category : 'declaring' }
ClyCommandInteraction >> add: aCommandClass order: aNumber [

	^ (self add: aCommandClass)
		  order: aNumber;
		  yourself
]

{ #category : 'declaring' }
ClyCommandInteraction >> addAllConcreteSubclassesOf: aCommandClass [
	"Adds every concrete subclass of a closed family of commands whose hierarchy is the menu, each with its own order. Answers the new entries."

	^ ((aCommandClass allSubclasses reject: [ :commandClass | commandClass isAbstract ]) sorted: [ :first :second | first name <= second name ]) collect: [ :commandClass | self add: commandClass ]
]

{ #category : 'accessing' }
ClyCommandInteraction >> entries [

	^ entries
]

{ #category : 'accessing' }
ClyCommandInteraction >> group [
	"The menu group class of the commands I add."

	^ group
]

{ #category : 'declaring' }
ClyCommandInteraction >> group: aMenuGroupClass [
	"Answers a view of me which adds its commands to aMenuGroupClass. It shares my entries."

	^ self copy setGroup: aMenuGroupClass
]

{ #category : 'accessing' }
ClyCommandInteraction >> labelOf: aCommand [
	"The label of a command whose entry has none."

	^ aCommand perform: labelSelector
]

{ #category : 'accessing' }
ClyCommandInteraction >> map [

	^ map
]

{ #category : 'accessing' }
ClyCommandInteraction >> name [

	^ name
]

{ #category : 'printing' }
ClyCommandInteraction >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: name;
		nextPutAll: ' of ';
		print: map anchor;
		nextPut: $)
]

{ #category : 'initialization' }
ClyCommandInteraction >> setGroup: aMenuGroupClass [

	group := aMenuGroupClass
]

{ #category : 'initialization' }
ClyCommandInteraction >> setName: aSymbol map: aCommandMap entryClass: aCommandEntryClass labelSelector: aSelector [

	name := aSymbol.
	map := aCommandMap.
	entryClass := aCommandEntryClass.
	labelSelector := aSelector.
	entries := OrderedCollection new.
	group := ClyRootMenuGroup
]

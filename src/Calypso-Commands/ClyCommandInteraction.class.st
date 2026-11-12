"
I am the commands of one interaction in a Calypso command map, such as its context menu, its toolbar or its shortcuts. Declaration methods add commands to me:
	aCommandMap contextMenu add: SycRemoveMethodCommand order: 10000.
	(aCommandMap contextMenu group: SycMethodRefactoringMenuGroup) add: SycPullUpMethodCommand.
	aCommandMap shortcuts add: SycRemoveMethodCommand key: $x meta.
	aCommandMap dragAndDrop add: SycMoveMethodsToClassCommand dropIn: ClyClass.
#group: answers a view of me which adds its commands to a menu group class. The views share my entries.
#addAllConcreteSubclassesOf: adds a whole family of commands. Use it only for closed families whose hierarchy is the menu, such as view mode switches. Otherwise add each command, so a command does not have to subclass a family to be shown.
#remove:, #removeMenuOf:, #rebind:to: and #rebindMenuOf:to: change the commands declared on my anchor by other packages. They are recorded and applied by #applyOverrides once all the declaration methods of the anchor ran.
My label selector is the message asked to a command for its label when its entry has none: #name for menus, #defaultToolbarItemName for the toolbar.
The interactions and the options of their entries are described in doc/Calypso/Commands.md of the Pharo repository.
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
		'entryClass',
		'overrides'
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

{ #category : 'declaring' }
ClyCommandInteraction >> addMenuOf: aMenuGroupClass [
	"Adds a command opening the commands of the context menu declared in the menu group, as a pop-up menu. Used for shortcuts and tab actions."

	^ (self add: ClyOpenMenuGroupCommand)
		  openedGroup: aMenuGroupClass;
		  yourself
]

{ #category : 'declaring' }
ClyCommandInteraction >> addMenuOf: aMenuGroupClass key: aKeyCombination [

	^ (self addMenuOf: aMenuGroupClass)
		  keyCombination: aKeyCombination;
		  yourself
]

{ #category : 'overriding' }
ClyCommandInteraction >> applyOverrides [
	"Applies the overrides in the order of their declaration methods. Each override remembers its method, so the entries it changes know it."

	overrides do: [ :override |
			map declaringMethod: override key.
			[ override value value ] ensure: [ map declaringMethod: nil ] ]
]

{ #category : 'private' }
ClyCommandInteraction >> ensureEntriesHaveKeys [

	(entryClass canUnderstand: #keyCombination:) ifFalse: [ self error: 'Only shortcuts have a key combination, not the entries of ' , name ]
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

{ #category : 'testing' }
ClyCommandInteraction >> hidesCommandsThatCannotRun [
	"Menus, toolbars, tab actions and table icons only show the commands that can run (#canBeExecuted). Shortcuts, clicks and drops run their command when the context allows it."

	^ #( #contextMenu #toolbar #tabActions #tableIcons #textLeftBarMenu ) includes: name
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

{ #category : 'private' }
ClyCommandInteraction >> overrideEntriesSuchThat: aSelectBlock with: anOverrideBlock [
	"Records an override, applied by #applyOverrides once all the declaration methods of my anchor ran, so the order of these methods does not matter."

	overrides add: map declaringMethod -> [
			(entries select: aSelectBlock) do: [ :entry |
					anOverrideBlock value: entry.
					entry overridingMethod: map declaringMethod ] ]
]

{ #category : 'accessing' }
ClyCommandInteraction >> overrides [
	"The overrides recorded by #remove:, #rebind:to:... and their declaration methods."

	^ overrides
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

{ #category : 'overriding' }
ClyCommandInteraction >> rebind: aCommandClass to: aKeyCombination [
	"Changes the key combination of the shortcuts of the command declared on my anchor, such as:
		aCommandMap shortcuts rebind: SycRemoveMethodCommand to: $d meta
	Applied after all the declaration methods of my anchor ran, see #remove:."

	self ensureEntriesHaveKeys.
	self overrideEntriesSuchThat: [ :entry | entry annotates: aCommandClass ] with: [ :entry | entry keyCombination: aKeyCombination ]
]

{ #category : 'overriding' }
ClyCommandInteraction >> rebindMenuOf: aMenuGroupClass to: aKeyCombination [
	"Changes the key combination of the shortcuts declared with #addMenuOf:key: which open the menu group, see #rebind:to:."

	self ensureEntriesHaveKeys.
	self overrideEntriesSuchThat: [ :entry | entry openedGroup = aMenuGroupClass ] with: [ :entry | entry keyCombination: aKeyCombination ]
]

{ #category : 'overriding' }
ClyCommandInteraction >> remove: aCommandClass [
	"Removes the entries of the command declared in this interaction of my anchor, whatever their menu group. Used to hide a command declared by another package. Like every override, it is applied after all the declaration methods of my anchor ran, and does nothing when the command is not declared there."

	self overrideEntriesSuchThat: [ :entry | entry annotates: aCommandClass ] with: [ :entry | entries remove: entry ]
]

{ #category : 'overriding' }
ClyCommandInteraction >> removeMenuOf: aMenuGroupClass [
	"Removes the entries declared with #addMenuOf: which open the menu group, see #remove:."

	self overrideEntriesSuchThat: [ :entry | entry openedGroup = aMenuGroupClass ] with: [ :entry | entries remove: entry ]
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
	overrides := OrderedCollection new.
	group := ClyRootMenuGroup
]

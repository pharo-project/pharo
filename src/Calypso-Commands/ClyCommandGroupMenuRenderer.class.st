"
I render a Commander2 command group (CmCommandGroup) into a Morphic menu (MenuMorph).

Each command of the group becomes a menu item using its name, description, icon and
canBeExecuted status. I am the Morphic counterpart of the Spec2-Commander2 renderer
(#asSpecGroup) and exist so Calypso can display Commander2 commands in its Morphic
context menus.
"
Class {
	#name : 'ClyCommandGroupMenuRenderer',
	#superclass : 'Object',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandGroupMenuRenderer class >> render: aCommandGroup on: aMenuMorph [

	^ self new
		  render: aCommandGroup
		  on: aMenuMorph
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> notebookActionsFor: aPresentedCommand [
	"The notebook action buttons of one command, nothing when it cannot run."
	| activator |
	aPresentedCommand isActive ifFalse: [ ^ #() ].
	activator := aPresentedCommand activator.
	^ { ClyNotebookPageActionButtonMorph forAction: (ClyNotebookPageAction
		action: [ activator executeCommand ]
		icon: aPresentedCommand command browserTabActionIcon
		label: activator commandDescription) }
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> notebookActionsOf: aPresentedRoot [
	"Answers the notebook actions of the tab tree, sorted flat over all groups."
	| sorted |
	sorted := (aPresentedRoot entries flatCollect: [ :group | group entries ]) asSortedCollection: [ :a :c |
		a order = c order ifTrue: [ a dynamicName <= c dynamicName ] ifFalse: [ a order <= c order ] ].
	^ sorted flatCollect: [ :each | self notebookActionsFor: each ]
]

{ #category : 'registering' }
ClyCommandGroupMenuRenderer >> registerGroup: aPresentedGroup on: aBuilder parentName: parentName actualName: actualName [
	"ClyMenuGroup>>registerContextMenuItemsWithBuilder:. The items of a group bind to the group itself when it is drawn as a sub-menu, and to the group they are inlined into otherwise."
	| descriptor ownName |
	aPresentedGroup isActive ifFalse: [ ^ self ].
	descriptor := aPresentedGroup descriptor.
	ownName := descriptor isInlined ifTrue: [ actualName ] ifFalse: [ aPresentedGroup dynamicName ].
	descriptor isInlined
		ifTrue: [ aBuilder items ifNotEmpty: [ aBuilder withSeparatorAfter ] ]
		ifFalse: [ self registerSubMenuOf: aPresentedGroup on: aBuilder parentName: parentName ].
	aPresentedGroup entries do: [ :each |
		each isGroup
			ifTrue: [ self registerGroup: each on: aBuilder parentName: aPresentedGroup dynamicName actualName: ownName ]
			ifFalse: [ self registerItem: each on: aBuilder inGroup: aPresentedGroup actualName: ownName ] ].
	(descriptor isInlined and: [ aBuilder items notEmpty ]) ifTrue: [ aBuilder withSeparatorAfter ]
]

{ #category : 'registering' }
ClyCommandGroupMenuRenderer >> registerItem: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName [
	"An item of a presented group: an activation is registered by its command (as its ActivatorMenuItem does), a pure command as a ClyCommandMenuItem."
	aPresentedCommand isActive ifFalse: [ ^ self ].
	aPresentedCommand activator
		ifNil: [ self registerPureItem: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName ]
		ifNotNil: [ :activator | aPresentedCommand command registerPresented: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName ]
]

{ #category : 'registering' }
ClyCommandGroupMenuRenderer >> registerPureItem: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName [
	"ClyCommandMenuItem>>registerContextMenuItemsWithBuilder:, for a pure command."
	| command itemOrder item |
	command := aPresentedCommand command.
	itemOrder := aPresentedCommand order.
	(aGroup descriptor isRoot not and: [ aGroup descriptor isInlined ]) ifTrue: [ itemOrder := aGroup order + (itemOrder / 10000000.0) ].
	item := (aBuilder item: aPresentedCommand dynamicName)
		parent: actualName;
		order: itemOrder;
		help: command description;
		action: [ command execute ];
		icon: command icon.
	ClyShortcutActivation
		activeInstancesFor: command class
		inContext: command context
		do: [ :shortcut |
			item keyText: (shortcut keyCombination acceptVisitor: OSPlatform current shortcutPrinter) ]
]

{ #category : 'registering' }
ClyCommandGroupMenuRenderer >> registerSubMenuOf: aPresentedGroup on: aBuilder parentName: parentName [
	"ClyMenuGroup>>registerContextSubMenuWithBuilder:."
	| descriptor |
	descriptor := aPresentedGroup descriptor.
	(aBuilder item: descriptor name)
		order: descriptor order;
		parent: parentName;
		target: aPresentedGroup;
		icon: descriptor icon;
		help: descriptor description
]

{ #category : 'registering' }
ClyCommandGroupMenuRenderer >> registerTree: aPresentedRoot on: aBuilder rootName: aName [
	"Registers the presented tree in a pragma (Rubric) menu builder, as ClyMenu>>registerContextMenuItemsWithBuilder: does. aName is the name the top items are bound to (the opener label for a sub-menu)."
	self registerGroup: aPresentedRoot on: aBuilder parentName: nil actualName: (aName ifNil: [ aPresentedRoot dynamicName ])
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> render: aCommandGroup on: aMenuMorph [

	aCommandGroup commands do: [ :command | self renderCommand: command on: aMenuMorph ].
	^ aMenuMorph
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderCommand: aCommand on: aMenuMorph [

	| item |
	item := aMenuMorph
		        add: aCommand name
		        target: aCommand
		        selector: #execute.
	item enabled: aCommand canBeExecuted.
	item setBalloonText: aCommand description.
	"To clean."
	(aCommand respondsTo: #icon) ifTrue: [
		aCommand icon ifNotNil: [ :anIcon | item icon: anIcon ] ].
	^ item
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderEntry: anEntry on: aMenu [
	anEntry isGroup
		ifTrue: [ self renderGroup: anEntry on: aMenu ]
		ifFalse: [ self renderItem: anEntry on: aMenu ]
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderGroup: aPresentedGroup on: aMenu [
	"Renders a group of the presented tree the way ClyMenuGroup>>buildContextMenu: does."
	| descriptor |
	descriptor := aPresentedGroup descriptor.
	descriptor isInlined
		ifTrue: [
			aMenu addLine.
			aPresentedGroup entries do: [ :each | self renderEntry: each on: aMenu ].
			aMenu addLine ]
		ifFalse: [
			descriptor hasLineBefore ifTrue: [ aMenu addLine ].
			self renderSubMenuOf: aPresentedGroup on: aMenu.
			descriptor hasLineAfter ifTrue: [ aMenu addLine ] ]
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderItem: aPresentedCommand on: aMenu [
	"Renders one command as a context menu item of its activation: the alternatives it stands for, or one item, with its sub-menu when it opens one."
	| activator command submenu item |
	aPresentedCommand isActive ifFalse: [ ^ self ].
	activator := aPresentedCommand activator.
	activator ifNil: [ ^ self renderCommand: aPresentedCommand command on: aMenu ].
	command := aPresentedCommand command.
	command menuAlternatives notEmpty
		ifTrue: [
			command menuAlternatives do: [ :each | | alternative |
				alternative := activator newForCommand: each.
				aMenu add: alternative menuItemName target: alternative selector: #executeCommand ].
			^ self ].
	submenu := command menuSubmenuFor: activator.
	(submenu notNil and: [ submenu hasItems not ]) ifTrue: [ ^ self ].
	item := aMenu add: activator menuItemName target: activator selector: #executeCommand.
	activator setUpShortcutTipForMenuItem: item.
	command setUpIconForMenuItem: item.
	command description ifNotNil: [ :text | item setBalloonText: text ].
	submenu ifNotNil: [ aMenu lastItem subMenu: submenu ]
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderPureMenuItems: commands on: aMenu placement: placement [
	"Draws the pure commands of an opener: appended to the menu, or put in front of its items keeping their order."
	placement = #prepend
		ifTrue: [
			| temp |
			temp := MenuMorph new.
			commands do: [ :each | self renderCommand: each on: temp ].
			temp items reverseDo: [ :each | aMenu addMorphFront: each ] ]
		ifFalse: [ commands do: [ :each | self renderCommand: each on: aMenu ] ]
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderSubMenuOf: aPresentedGroup on: aMenu [
	"Renders a group as a sub-menu item, as ClyMenuGroup>>buildContextSubMenuIn: does."
	| descriptor submenu |
	descriptor := aPresentedGroup descriptor.
	submenu := MorphicUIManager new newMenuIn: aMenu defaultTarget for: aMenu defaultTarget.
	aPresentedGroup entries do: [ :each | self renderEntry: each on: submenu ].
	submenu hasItems ifTrue: [
		aMenu add: descriptor name icon: descriptor icon subMenu: submenu.
		descriptor description ifNotNil: [ :text | aMenu lastItem setBalloonText: text ] ]
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderToolbarItem: aPresentedCommand on: aToolbarMorph [
	"Adds the toolbar item of one active command: its widget, or a button for its activator, decorated with its balloon and icon when it asks for it."
	| activator command item |
	aPresentedCommand isActive ifFalse: [ ^ self ].
	activator := aPresentedCommand activator.
	command := aPresentedCommand command.
	item := (command toolbarItemFor: activator) ifNil: [ ClyBrowserButtonMorph activatingBy: activator ].
	command decoratesToolbarItem ifTrue: [
		command description ifNotNil: [ :text | item setBalloonText: text ].
		command defaultMenuIconFormSet ifNotNil: [ :iconFormSet | item addIcon: iconFormSet asMorph ] ].
	aToolbarMorph addNewItem: item
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderToolbarOf: aPresentedRoot on: aToolbarMorph [
	"Draws the toolbar tree: a separator before each group that has an active entry, then each entry in order."
	aPresentedRoot entries do: [ :group |
		(group entries anySatisfy: [ :each | each isActive ]) ifTrue: [
			aToolbarMorph submorphs ifNotEmpty: [ aToolbarMorph addNewItem: ClyToolbarSeparatorMorph new ].
			group entries do: [ :each | self renderToolbarItem: each on: aToolbarMorph ] ] ]
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderTree: aPresentedRoot on: aMorph [
	"Answers the Morphic context menu of the presented tree, as ClyMenu>>buildContextMenuFor: does: the root is drawn inline, then a trailing line is removed."
	| menu |
	menu := MorphicUIManager new newMenuIn: aMorph for: aMorph.
	self renderGroup: aPresentedRoot on: menu.
	menu hasSubmorphs ifTrue: [
		menu lastSubmorph isMenuLineMorph ifTrue: [ menu lastSubmorph delete ] ].
	^ menu
]

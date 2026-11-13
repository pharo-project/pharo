"
I am the base of the commands which open a context menu of a Calypso tool. My subclasses provide the activation strategy whose commands make up the menu, and I open the menu on the context I was executed in.
"
Class {
	#name : 'ClyOpenContextMenuCommand',
	#superclass : 'ClyCommand',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'testing' }
ClyOpenContextMenuCommand class >> isAbstract [
	^self = ClyOpenContextMenuCommand
]

{ #category : 'execution' }
ClyOpenContextMenuCommand >> activationStrategy [
	"Subclasses should return specific kind of activation strategy
	which will be used to collect self context menu commands"
	self subclassResponsibility
]

{ #category : 'registering' }
ClyOpenContextMenuCommand >> addPureItemsTo: aPresentedRoot [
	"Hook: the pure commands an opener adds to its sub-menu. None by default."
	^ self
]

{ #category : 'building' }
ClyOpenContextMenuCommand >> createContextMenu [
	"The activation part of the sub-menu, drawn from the Commander2 tree, then the pure commands of this opener."
	| menu |
	menu := ClyCommandGroupMenuRenderer new
		renderTree: (ClyCommandGroupBuilder new buildMenuFor: self activationStrategy context: self context pureRoots: #())
		on: self context tool.
	ClyCommandGroupMenuRenderer new
		renderPureMenuItems: self pureMenuCommands
		on: menu
		placement: self pureMenuPlacement.
	^ menu
]

{ #category : 'execution' }
ClyOpenContextMenuCommand >> execute [

	| menu |
	menu := self createContextMenu.
	menu hasItems ifFalse: [^self].

	menu setInvokingView: self context tool.
	menu invokeModal
]

{ #category : 'menu' }
ClyOpenContextMenuCommand >> menuSubmenuFor: aCommandActivator [
	"The menu of this opener, built from its own activations and pure commands. It stays empty while this command is being built, which is how the recursion through this opener is stopped."
	| submenu |
	submenu := MenuMorph new.
	RecursionStopper during: [ submenu := self createContextMenu ].
	^ submenu
]

{ #category : 'accessing' }
ClyOpenContextMenuCommand >> name [
	^ 'Open self context menu'
]

{ #category : 'menu' }
ClyOpenContextMenuCommand >> pureMenuCommands [
	"The pure commands this opener adds to its menu. None by default."
	^ #()
]

{ #category : 'menu' }
ClyOpenContextMenuCommand >> pureMenuPlacement [
	"Where the pure commands go: #append after the activations, or #prepend in front of them."
	^ #append
]

{ #category : 'registering' }
ClyOpenContextMenuCommand >> registerPresented: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName [
	"The sub-menu of an opener in a pragma menu builder: registered only when its activations are active (the pure commands of addPureItemsTo: do not count), under the opener label."
	RecursionStopper during: [ | submenu |
		submenu := ClyCommandGroupBuilder new buildMenuFor: self activationStrategy context: self context pureRoots: #().
		submenu isActive ifTrue: [
			self registerActivationItem: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName.
			self addPureItemsTo: submenu.
			ClyCommandGroupMenuRenderer new registerTree: submenu on: aBuilder rootName: aPresentedCommand dynamicName ] ]
]

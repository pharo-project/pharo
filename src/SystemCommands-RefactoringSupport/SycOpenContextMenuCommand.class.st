"
I am a command to open special context menu.
I show in menu all commands annotated by activationStrategy class
"
Class {
	#name : 'SycOpenContextMenuCommand',
	#superclass : 'ClyOpenContextMenuCommand',
	#category : 'SystemCommands-RefactoringSupport',
	#package : 'SystemCommands-RefactoringSupport'
}

{ #category : 'registering' }
SycOpenContextMenuCommand >> addPureItemsTo: aPresentedRoot [
	"The pure commands of cmCommandClass are added to the sub-menu of a pragma menu builder only when their own activation strategy is the strategy of this sub-menu, as addItemsToSubmenu:forContext: did."
	| commands |
	self cmCommandClass activationStrategy = self activationStrategy ifFalse: [ ^ self ].
	commands := self cmCommandClass subclasses
		collect: [ :each | each new context: self context; yourself ]
		thenSelect: [ :each | each canBeExecuted ].
	commands do: [ :each |
		aPresentedRoot register: (ClyPresentedCommand new
			decorate: each;
			identifier: each class name;
			yourself) ]
]

{ #category : 'menu' }
SycOpenContextMenuCommand >> classFamilyPureMenuCommands [
	"The commands of the class of this opener that apply here, in the order they are built (for the class-family openers)."
	^ (ClyCommandGroupBuilder buildFor: self cmCommandClass context: self context) commands
]

{ #category : 'context menu' }
SycOpenContextMenuCommand >> cmCommandClass [
	self subclassResponsibility
]

{ #category : 'menu' }
SycOpenContextMenuCommand >> pureMenuCommands [
	"The commands of the class of this opener that apply here (the generic drawing of addMenuItemsTo:onContext:)."
	^ self cmCommandClass allSubclasses
		collect: [ :each | each new context: self context; yourself ]
		thenSelect: [ :each | each canBeExecuted ]
]

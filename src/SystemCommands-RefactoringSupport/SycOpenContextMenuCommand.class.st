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

{ #category : 'execution' }
SycOpenContextMenuCommand >> addClyCommandsToMenu: aMenuMorph [

	self cmCommandClass
		addMenuItemsTo: aMenuMorph
		onContext: self context
]

{ #category : 'context menu' }
SycOpenContextMenuCommand >> cmCommandClass [
	self subclassResponsibility
]

{ #category : 'execution' }
SycOpenContextMenuCommand >> createContextMenu [

	| menuMorph |
	menuMorph := super createContextMenu.
	self addClyCommandsToMenu: menuMorph.
	^ menuMorph
]

{ #category : 'context menu' }
SycOpenContextMenuCommand >> registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder [

	"Here we need to take care because this command itself is added to menu of same context.
	So we will always enter into this method during submenu generation.
	We catch this recursion using RecursionStopper.
	It is easier than adding extra logic to menu registration process"

	| submenu |
	RecursionStopper during: [
		submenu := self activationStrategy buildMenuInContext: context.
		submenu isActive ifTrue: [
			self
				doRegisterContextMenuItemsFor: aCommandItem
				withBuilder: aBuilder.
			self cmCommandClass addItemsToSubmenu: submenu forContext: context.

			"Registration builder uses #name to bind children to parent"
			submenu rootGroup name: aCommandItem name.
			submenu registerContextMenuItemsWithBuilder: aBuilder ] ]
]

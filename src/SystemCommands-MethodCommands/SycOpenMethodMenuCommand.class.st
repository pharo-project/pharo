"
I am a command to open special method menu.
I show in menu all commands annotated by SycMethodMenuActivation
"
Class {
	#name : #SycOpenMethodMenuCommand,
	#superclass : #CmdOpenContextMenuCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycOpenMethodMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $t meta for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
SycOpenMethodMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: -10 for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycOpenMethodMenuCommand >> activationStrategy [ 
	^ SycMethodMenuActivation
]

{ #category : #'context menu' }
SycOpenMethodMenuCommand >> createContextMenu [ 

	| menuMorph |
	
	menuMorph := super createContextMenu.
	SycMethodCmCommand addMenuItemsTo: menuMorph onContext: self context.
	^ menuMorph 
]

{ #category : #accessing }
SycOpenMethodMenuCommand >> defaultMenuItemName [
	^'Refactorings'
]

{ #category : #execution }
SycOpenMethodMenuCommand >> execute [

	context selectedTextInterval ifEmpty: [ 
		context showSourceNode].
	
	super execute
]

{ #category : #'context menu' }
SycOpenMethodMenuCommand >> registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder [
	"Here we need to take care because this command itself is added to menu of same context.
	So we will always enter into this method during submenu generation. 
	We catch this recursion using RecursionStopper. 
	It is easier than adding extra logic to menu registration process"	
	| submenu |
	RecursionStopper during: [ 		
		submenu := self activationStrategy buildMenuInContext: context.		 
		submenu isActive ifTrue: [
			self doRegisterContextMenuItemsFor: aCommandItem withBuilder: aBuilder.
								
			SycMethodCmCommand addItemsToSubmenu: submenu forContext: context.

			"Registration builder uses #name to bind children to parent"
			submenu rootGroup name: aCommandItem name.
			submenu registerContextMenuItemsWithBuilder: aBuilder].
	]
]

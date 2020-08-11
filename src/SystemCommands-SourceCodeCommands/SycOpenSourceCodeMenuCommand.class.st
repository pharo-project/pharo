"
I am a command to open special source code menu.
I show in menu all commands annotated by SycSourceCodeMenuActivation
"
Class {
	#name : #SycOpenSourceCodeMenuCommand,
	#superclass : #CmdOpenContextMenuCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycOpenSourceCodeMenuCommand >> activationStrategy [
	^SycSourceCodeMenuActivation 
]

{ #category : #execution }
SycOpenSourceCodeMenuCommand >> createContextMenu [ 

	| menuMorph |
	
	menuMorph := super createContextMenu.
	SycSourceCodeCmCommand addMenuItemsTo: menuMorph onContext: self context.
	^ menuMorph 
]

{ #category : #accessing }
SycOpenSourceCodeMenuCommand >> defaultMenuIconName [
	^#smallAuthoringTools
]

{ #category : #accessing }
SycOpenSourceCodeMenuCommand >> defaultMenuItemName [
	^'Source code'
]

{ #category : #execution }
SycOpenSourceCodeMenuCommand >> execute [

	context selectedTextInterval ifEmpty: [ 
		context showSourceNode].
	
	super execute
]

{ #category : #'context menu' }
SycOpenSourceCodeMenuCommand >> registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder [
	"Here we need to take care because this command itself is added to menu of same context.
	So we will always enter into this method during submenu generation. 
	We catch this recursion using RecursionStopper. 
	It is easier than adding extra logic to menu registration process"	
	| submenu |
	RecursionStopper during: [ 		
		submenu := self activationStrategy buildMenuInContext: context.		 
		submenu isActive ifTrue: [
			self doRegisterContextMenuItemsFor: aCommandItem withBuilder: aBuilder.
								
			SycSourceCodeCmCommand addItemsToSubmenu: submenu forContext: context.

			"Registration builder uses #name to bind children to parent"
			submenu rootGroup name: aCommandItem name.
			submenu registerContextMenuItemsWithBuilder: aBuilder].
	]
]

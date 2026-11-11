"
I am a command to open special source code menu.
I show in menu all commands annotated by SycSourceCodeMenuActivation
"
Class {
	#name : 'SycOpenSourceCodeMenuCommand',
	#superclass : 'SycOpenContextMenuCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycOpenSourceCodeMenuCommand >> activationStrategy [
	^SycSourceCodeMenuActivation
]

{ #category : 'context menu' }
SycOpenSourceCodeMenuCommand >> cmCommandClass [

	^ SycSourceCodeCmCommand
]

{ #category : 'context menu' }
SycOpenSourceCodeMenuCommand >> cmCommandClasses [

	^ {
		  SycSourceCodeCmCommand.
		  SycSourceCodeRefactoringCommand.
		  SycRefactorVariableCommand.
		  SycMessageCmCommand.
		  ClyMethodEditorCommand.
		  SycVariableNavigationCmCommand.
		  SycSingleClassCommand.
		  SycClassCommand.
		  SycChangeMessageMenuCmCommand.
		  SycMessageCommand }
]

{ #category : 'building' }
SycOpenSourceCodeMenuCommand >> createContextMenu [
	"The source-code menu: the activations and the pure commands of cmCommandClasses, built as the Commander2 tree and drawn by its renderer."
	^ ClyCommandGroupMenuRenderer new
		renderTree: (ClyCommandGroupBuilder new buildMenuFor: self activationStrategy context: self context pureRoots: self cmCommandClasses)
		on: self context tool
]

{ #category : 'accessing' }
SycOpenSourceCodeMenuCommand >> description [

	^ 'Opens special source code menu.'
]

{ #category : 'execution' }
SycOpenSourceCodeMenuCommand >> execute [

	self context selectedTextInterval ifEmpty: [
		self context showSourceNode].

	super execute
]

{ #category : 'accessing' }
SycOpenSourceCodeMenuCommand >> iconName [
	^#smallAuthoringTools
]

{ #category : 'accessing' }
SycOpenSourceCodeMenuCommand >> name [
	^'Source code'
]

{ #category : 'registering' }
SycOpenSourceCodeMenuCommand >> registerPresented: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName [
	"The source-code sub-menu in a pragma menu builder: the activations and the pure commands of cmCommandClasses."
	RecursionStopper during: [ | submenu |
		submenu := ClyCommandGroupBuilder new buildMenuFor: self activationStrategy context: self context pureRoots: self cmCommandClasses.
		submenu isActive ifTrue: [
			self registerActivationItem: aPresentedCommand on: aBuilder inGroup: aGroup actualName: actualName.
			ClyCommandGroupMenuRenderer new registerTree: submenu on: aBuilder rootName: aPresentedCommand dynamicName ] ]
]

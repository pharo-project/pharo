"
I check that a theme finds the tools behind its file dialogs in its tool registry, which the IDE fills.
"
Class {
	#name : 'UIThemeDialogsTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Tests-Widgets',
	#package : 'Morphic-Tests',
	#tag : 'Widgets'
}

{ #category : 'tests' }
UIThemeDialogsTest >> testDialogsComeFromTheToolRegistry [

	| theme registry |
	registry := ToolRegistry new.
	registry
		register: Object as: #fileDialog;
		register: Set as: #fileSaveDialog;
		register: Bag as: #directoryDialog.
	theme := UITheme current class new.
	theme tools: registry.

	self assert: theme fileDialogWindowClass equals: Object.
	self assert: theme fileSaveDialogWindowClass equals: Set.
	self assert: theme directoryDialogWindowClass equals: Bag
]

{ #category : 'tests' }
UIThemeDialogsTest >> testFontChooserComesFromTheToolRegistry [

	| theme registry |
	registry := ToolRegistry new.
	registry register: Object as: #fontChooser.
	theme := UITheme current class new.
	theme tools: registry.

	self assert: theme fontChooserClass equals: Object
]

{ #category : 'tests' }
UIThemeDialogsTest >> testNoRegisteredDialogIsAnError [
	"A fresh registry holds only the tools that register themselves: the IDE registers the dialogs"

	| theme registry |
	registry := ToolRegistry new.
	theme := UITheme current class new.
	theme tools: registry.

	self should: [ theme fileSaveDialogWindowClass ] raise: Error
]

"
Command to find selected text, to be used as editing method package
"
Class {
	#name : 'ClyBrowseMethodsContainingStringCommand',
	#superclass : 'ClyMethodEditorCommand',
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'testing' }
ClyBrowseMethodsContainingStringCommand class >> canBeExecutedInMethodEditor: aTool [

	^ true
]

{ #category : 'activation' }
ClyBrowseMethodsContainingStringCommand class >> methodEditorShorcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $e meta, $m meta, $s meta for: ClyTextEditorContext
]

{ #category : 'accessing' }
ClyBrowseMethodsContainingStringCommand >> defaultMenuItemName [

	^'Method source with it'
]

{ #category : 'execution' }
ClyBrowseMethodsContainingStringCommand >> execute [

	| selectedText |
	
	selectedText := methodEditor selectedSourceNode sourceCode.
	self systemNavigation 
		browseMethodsWithSourceString: selectedText
		matchCase: false
	 
]

"
I am a command to invert hierarhcy in the method visibility class list.
(it is shown when user expands first item in the method groups pane)
"
Class {
	#name : #ClyInvertMethodVisibilityHierarchyCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #activation }
ClyInvertMethodVisibilityHierarchyCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyInheritedMethodGroup asCalypsoItemContext
]

{ #category : #accessing }
ClyInvertMethodVisibilityHierarchyCommand >> defaultMenuItemName [ 
	^'Invert hierarchy'
]

{ #category : #execution }
ClyInvertMethodVisibilityHierarchyCommand >> execute [
	ClySubclassHierarchy invertMethodVisibilityHierarchy.
	browser switchToMethodGroups
]

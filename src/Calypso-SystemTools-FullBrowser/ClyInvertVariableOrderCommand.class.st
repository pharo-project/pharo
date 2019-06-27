"
I am a command to invert hierarhcy of variable list in full browser.
By default variables are ordered from superclass to subclass. And I allow to toggle this order
"
Class {
	#name : #ClyInvertVariableOrderCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #activation }
ClyInvertVariableOrderCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyVariableMethodGroup asCalypsoItemContext
]

{ #category : #accessing }
ClyInvertVariableOrderCommand >> defaultMenuItemName [ 
	^'Invert hierarchy'
]

{ #category : #execution }
ClyInvertVariableOrderCommand >> execute [

	ClySubclassHierarchy invertVariableHierarchy.
	browser switchToVariables 
]

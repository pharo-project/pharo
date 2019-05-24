"
I am a command to show all method implementors in full hierarchy of selected method classes.
I select current method in spawned browser by default.
"
Class {
	#name : #ClyShowMethodInheritanceCommand,
	#superclass : #ClyShowLocalImplementorsCommand,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Browser'
}

{ #category : #activation }
ClyShowMethodInheritanceCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyMethod asCalypsoItemContext 
]

{ #category : #activation }
ClyShowMethodInheritanceCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $h meta for: ClyMethod asCalypsoItemContext 
]

{ #category : #execution }
ClyShowMethodInheritanceCommand >> createInheritanceScopeFrom: classes [

	| classScope |
	classScope := ClyClassScope ofAll: classes in: browser navigationEnvironment.
	
	^classScope asFullHierarchyScope
]

{ #category : #accessing }
ClyShowMethodInheritanceCommand >> defaultMenuItemName [ 
	^'Inheritance'
]

{ #category : #execution }
ClyShowMethodInheritanceCommand >> selectMethodsIn: aQueryBrowser [

	aQueryBrowser selectObject: methods last
]

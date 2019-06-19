"
I am a command to switch the full browser to full hierarchy mode of selected classes
"
Class {
	#name : #ClySwitchToFullHierarchyCommand,
	#superclass : #ClySwitchClassViewModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Hierarchy'
}

{ #category : #activation }
ClySwitchToFullHierarchyCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $h meta for: ClyClassContextOfFullBrowser
]

{ #category : #accessing }
ClySwitchToFullHierarchyCommand >> defaultMenuItemName [
	^'Hier.'
]

{ #category : #execution }
ClySwitchToFullHierarchyCommand >> execute [
	
	browser switchToFullClassHierarchy
]

{ #category : #testing }
ClySwitchToFullHierarchyCommand >> isAppliedToBrowser [

	^browser showsFullClassHierarchy
]

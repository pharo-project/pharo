"
I am a command to switch the full browser to full hierarchy mode of selected classes
"
Class {
	#name : 'ClySwitchToFullHierarchyCommand',
	#superclass : 'ClySwitchClassViewModeCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Hierarchy',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Hierarchy'
}

{ #category : 'activation' }
ClySwitchToFullHierarchyCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $h meta for: ClyFullBrowserClassContext
]

{ #category : 'execution' }
ClySwitchToFullHierarchyCommand >> execute [

	browser switchToFullClassHierarchy
]

{ #category : 'testing' }
ClySwitchToFullHierarchyCommand >> isAppliedToBrowser [

	^browser showsFullClassHierarchy
]

{ #category : 'accessing' }
ClySwitchToFullHierarchyCommand >> name [
	^'Hier.'
]

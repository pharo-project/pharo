"
I represent a global context of World.
My tool is a current World morph instance.

Users can use me as context for commands which should be available in world menu or accessible by global shortcut. For example: 

	SycOpenBrowserCommand class>>worldMenuActivation
		<classAnnotation>
		^CmdContextMenuCommandActivation byRootGroupItemOrder: -1 for: CmdWorldMenuContext 
		
	SycOpenBrowserCommand class>>shortcutActivation
		<classAnnotation>
		^CmdShortcutCommandActivation by: $b meta for: CmdWorldMenuContext 
"
Class {
	#name : #CmdWorldMenuContext,
	#superclass : #CmdToolContext,
	#category : #'Commander-Activators-WorldMenu'
}

"
I am a copy of class ClyInstallMetaLinkCommand. This comment is copied from there, and might not be entirely accurate

I am a command to install metalinks on the selected method or one of its ast nodes.
I open a small browser to choose which metalink to install among existing metalink instances.
"
Class {
	#name : #ClyUninstallMetaLinkCommand,
	#superclass : #ClyDebuggingCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Metalinks'
}

{ #category : #activation }
ClyUninstallMetaLinkCommand class >> contextMenuOrder [
	^101
]

{ #category : #accessing }
ClyUninstallMetaLinkCommand >> defaultMenuIconName [
	^#smallObjects
]

{ #category : #accessing }
ClyUninstallMetaLinkCommand >> defaultMenuItemName [
	^'Uninstall MetaLink...'
]

{ #category : #execution }
ClyUninstallMetaLinkCommand >> execute [
	ClyMetaLinkInstallationPresenter openUninstallerOnNode: sourceNode
]

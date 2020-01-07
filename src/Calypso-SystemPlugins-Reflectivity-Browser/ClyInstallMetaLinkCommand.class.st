"
I am a command to install metalinks on the selected method or one of its ast nodes.
I open a small browser to choose which metalink to install among existing metalink instances.
"
Class {
	#name : #ClyInstallMetaLinkCommand,
	#superclass : #ClyDebuggingCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Metalinks'
}

{ #category : #activation }
ClyInstallMetaLinkCommand class >> contextMenuOrder [
	^100
]

{ #category : #accessing }
ClyInstallMetaLinkCommand >> defaultMenuIconName [
	^#smallObjects
]

{ #category : #accessing }
ClyInstallMetaLinkCommand >> defaultMenuItemName [
	^'Install MetaLink...'
]

{ #category : #execution }
ClyInstallMetaLinkCommand >> execute [
	ClyMetaLinkInstallationPresenter openInstallerOnNode: sourceNode
]

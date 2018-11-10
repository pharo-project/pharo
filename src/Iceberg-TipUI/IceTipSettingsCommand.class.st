"
Command that opens the settings on a repository
"
Class {
	#name : #IceTipSettingsCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipSettingsCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $n meta for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipSettingsCommand class >> defaultHelp [
	^ 'Open settings browser to configure Iceberg'
]

{ #category : #accessing }
IceTipSettingsCommand class >> defaultMenuIconName [
	^ #configuration
]

{ #category : #accessing }
IceTipSettingsCommand class >> defaultMenuItemName [
	^ 'Settings'
]

{ #category : #accessing }
IceTipSettingsCommand class >> defaultPosition [
	^ #right
]

{ #category : #accessing }
IceTipSettingsCommand class >> toolbarRepositoryListActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipRepositoryListContext
		order: 10
]

{ #category : #execution }
IceTipSettingsCommand >> execute [
	| browser window path |
	
	browser := SettingBrowser new 
		changePackageSet: { 
			#'Iceberg' asPackage.
			#'Iceberg-Libgit' asPackage };
		yourself.
		 
	window := browser open.
	path := (window submorphs detect: [ :each | (each isKindOf: MorphTreeMorph)  ]) nodeList.
	browser expandNodePath: path first contents first contents first path.
]

"
I am a context of selected packages in full browser
"
Class {
	#name : 'ClyFullBrowserPackageContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'commands' }
ClyFullBrowserPackageContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tabs add: ClyClassCreationToolMorph.
	(aCommandMap toolbar group: ClyPackageToolbarGroup)
		add: ClySwitchToPackagesCommand order: 1;
		add: ClySwitchToProjectsCommand order: 10;
		add: ClySwitchToScopedViewCommand order: 1.
	aCommandMap shortcuts
		add: SycDuplicatePackageCommand key: $c meta;
		add: SycRemovePackageCommand key: $x meta;
		add: SycRenamePackageCommand key: $r meta.
	aCommandMap contextMenu
		add: SycAddNewClassTagCommand;
		add: SycAddNewPackageCommand;
		add: SycAddPackagesScopeCommand;
		add: SycDuplicatePackageCommand;
		add: SycRemovePackageCommand;
		add: SycRenamePackageCommand
]

{ #category : 'selection strategy' }
ClyFullBrowserPackageContext class >> selectionContextDeclaration [

	<calypsoSelectionContext>
	^ ClySelectionContextDeclaration browser: ClyFullBrowserMorph itemType: Package
]

{ #category : 'selection-class groups' }
ClyFullBrowserPackageContext >> isClassGroupSelected [
	^false
]

{ #category : 'testing' }
ClyFullBrowserPackageContext >> isFullBrowserPackageContext [

	^ true
]

{ #category : 'selection-class groups' }
ClyFullBrowserPackageContext >> isPackageTagSelected [
	^false
]

{ #category : 'public' }
ClyFullBrowserPackageContext >> lastSelectedClassGroup [
	^self lastSelectedObject
]

{ #category : 'selection-packages' }
ClyFullBrowserPackageContext >> selectedPackageItems [
	^selectedItems
]

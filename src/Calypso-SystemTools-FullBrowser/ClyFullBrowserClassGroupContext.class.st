"
I am a context of selected class groups in full browser
"
Class {
	#name : 'ClyFullBrowserClassGroupContext',
	#superclass : 'ClyFullBrowserContext',
	#category : 'Calypso-SystemTools-FullBrowser-Contexts',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Contexts'
}

{ #category : 'commands' }
ClyFullBrowserClassGroupContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap toolbar group: ClyPackageToolbarGroup)
		add: ClySwitchToPackagesCommand order: 1;
		add: ClySwitchToProjectsCommand order: 10;
		add: ClySwitchToScopedViewCommand order: 1.
	aCommandMap shortcuts
		add: ClyRemoveClassGroupCommand key: $x meta;
		add: ClyRenamePackageTagCommand key: $r meta.
	aCommandMap contextMenu
		add: ClyRemoveClassGroupCommand;
		add: ClyRenamePackageTagCommand;
		add: SycAddNewClassTagCommand;
		add: SycAddNewPackageCommand;
		add: SycAddPackagesScopeCommand;
		add: SycDuplicatePackageCommand;
		add: SycRemovePackageCommand;
		add: SycRenamePackageCommand
]

{ #category : 'selection strategy' }
ClyFullBrowserClassGroupContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyFullBrowserMorph selectionType: ClyClassGroup
]

{ #category : 'testing' }
ClyFullBrowserClassGroupContext >> isClassGroupSelected [
	^self hasSelectedItems
]

{ #category : 'testing' }
ClyFullBrowserClassGroupContext >> isFullBrowserClassGroupContext [

	^ true
]

{ #category : 'testing' }
ClyFullBrowserClassGroupContext >> isPackageSelected [
	^tool packageSelection isEmpty not
]

{ #category : 'testing' }
ClyFullBrowserClassGroupContext >> isPackageTagSelected [
	| classGroupClass |
	self isClassGroupSelected ifFalse: [ ^false ].

	classGroupClass := self lastSelectedItem type.

	^classGroupClass isBasedOnPackageTag
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> lastSelectedClassGroup [
	^self lastSelectedObject
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> lastSelectedPackageTag [

	^self lastSelectedObjectIn: self selectedPackageTags
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> selectedClassGroups [
	^self selectedObjects
]

{ #category : 'selection-class groups' }
ClyFullBrowserClassGroupContext >> selectedPackageTags [

	^selectedItems
		select: [ :each | each type isBasedOnPackageTag ]
		thenCollect: [ :each | each name ]
]

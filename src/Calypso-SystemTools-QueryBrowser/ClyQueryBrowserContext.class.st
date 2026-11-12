"
I represent query browser context
"
Class {
	#name : 'ClyQueryBrowserContext',
	#superclass : 'ClySystemBrowserContext',
	#category : 'Calypso-SystemTools-QueryBrowser-UI',
	#package : 'Calypso-SystemTools-QueryBrowser',
	#tag : 'UI'
}

{ #category : 'commands' }
ClyQueryBrowserContext class >> calypsoSystemToolsQueryBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClySwitchScopeMenuGroup) add: ClySwitchQueryScopeCommand.
	(aCommandMap toolbar group: ClyButtonToolbarGroup)
		add: ClyShowClassRefCommand order: 1000;
		add: ClySpawnFullBrowserCommand order: 1000.
	(aCommandMap toolbar group: ClyQueryResultToolbarGroup)
		add: ClySwitchQueryScopeCommand order: -5;
		addAllConcreteSubclassesOf: ClySwitchQueryResultCommand.
	aCommandMap shortcuts add: ClyShowClassRefCommand key: $c meta , $n meta.
	aCommandMap doubleClick add: ClySpawnFullBrowserCommand.
	aCommandMap contextMenu
		add: SycAbstractAllInstVarAccessorsCommand;
		add: SycAddNewProtocolCommand;
		add: SycCategorizeAllUnclassifiedMethodsCommand;
		add: SycGenerateAllInstVarAccessorsCommand;
		add: SycRealizeClassCommand;
		add: SycRemoveClassCommand;
		add: SycRemoveEmptyMethodTagsCommand.
	(aCommandMap contextMenu group: ClyQueryMenuGroup)
		add: ClyShowClassRefCommand;
		add: ClyShowPoolUsersCommand;
		add: ClyShowTraitUsersCommand
]

{ #category : 'selection strategy' }
ClyQueryBrowserContext class >> selectionContextDeclaration [

	<calypsoSelectionContext>
	^ ClySelectionContextDeclaration browser: ClyQueryBrowserMorph
]

{ #category : 'selection-classes' }
ClyQueryBrowserContext >> isClassSelected [
	"here we extract selected classes from selected items"
	^self hasSelectedItems
]

{ #category : 'selection-methods' }
ClyQueryBrowserContext >> isMethodSelected [
	"optimized version without extra collection copy"

	^selectedItems anySatisfy: [ :each | each isBasedOnItemType: ClyMethod  ]
]

{ #category : 'selection-packages' }
ClyQueryBrowserContext >> isPackageSelected [
	"here we extract selected packages from selected items"
	^self hasSelectedItems
]

{ #category : 'testing' }
ClyQueryBrowserContext >> isQueryBrowserContext [

	^ true
]

{ #category : 'selection-classes' }
ClyQueryBrowserContext >> lastSelectedClass [

	^self lastSelectedItem systemDefinition definingClass
]

{ #category : 'selection-packages' }
ClyQueryBrowserContext >> lastSelectedPackage [

	^self lastSelectedItem systemDefinition definingPackage
]

{ #category : 'browser state' }
ClyQueryBrowserContext >> restoreBrowserState [

	self browser selectObject: self lastSelectedObject
]

{ #category : 'selection-classes' }
ClyQueryBrowserContext >> selectedClassItems [

	| unique classItem |
	unique := OrderedCollection new.
	selectedItems do: [ :each |
		classItem := each systemDefinition definingClassItem.
		unique
			detect: [:existing | existing actualObject == classItem actualObject]
			ifNone: [unique add: classItem]].
	^unique
]

{ #category : 'selection-classes' }
ClyQueryBrowserContext >> selectedClassSide [
	^self lastSelectedClass
]

{ #category : 'selection-methods' }
ClyQueryBrowserContext >> selectedMethodItems [
	^selectedItems select: [ :each | each isBasedOnItemType: ClyMethod ]
]

{ #category : 'selection-packages' }
ClyQueryBrowserContext >> selectedPackageItems [

	| unique packageItem |
	unique := OrderedCollection new.
	selectedItems do: [ :each |
		packageItem := each systemDefinition definingPackageItem.
		unique
			detect: [:existing | existing actualObject == packageItem actualObject]
			ifNone: [unique add: packageItem]].
	^unique
]

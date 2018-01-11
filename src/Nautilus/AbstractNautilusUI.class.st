"
An AbstractNautilusUI is an abstraction of the UI handling packages and classes.

Instance Variables
	cachedHierarchy:		<Object>
	classesSelection:		<Object>
	commentButton:		<Object>
	commentTextArea:		<Object>
	currentDisplayChoice:		<Object>
	firstColumn:		<Object>
	groupsSelection:		<Object>
	hierarchyClass:		<Object>
	list:		<Object>
	list2:		<Object>
	model:		<Object>
	packagesSelection:		<Object>
	secondColumn:		<Object>
	sourceCodeContainer:		<Object>
	sourceCodePanel:		<Object>
	sourceTextAreaLimit:		<Object>
	window:		<Object>
"
Class {
	#name : #AbstractNautilusUI,
	#superclass : #AbstractTool,
	#instVars : [
		'model',
		'window',
		'sourceCodePanel',
		'fullDisplayedComment',
		'sourceCodeContainer',
		'shouldUpdateTitle',
		'contentSelection',
		'acceptor',
		'commentWidget',
		'sourceTextWidget',
		'sourceCodeSelectionInterval',
		'sourceTextModel',
		'methodClassifier',
		'commentTextModel',
		'instanceButtonWidget'
	],
	#classVars : [
		'NextFocusKey',
		'PackagesIconsCache',
		'PreviousFocusKey'
	],
	#category : #Nautilus
}

{ #category : #menu }
AbstractNautilusUI class >> analyzeSubMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalClassMenu>
	| target |
	
	target := aBuilder model.
	target selectedClass ifNil: [^target].
	target selectedClass buildAnalyzeSubMenu: aBuilder.
]

{ #category : #'icons behavior' }
AbstractNautilusUI class >> arrowDown: aMethod [

	| methods methodsNames index |
	methods := aMethod methodClass allSubclasses flatCollect: [:each | each methods ].
	methods := methods select: [:each | each selector = aMethod selector ].
	methods size = 1
		 ifTrue: [ ^ Nautilus openOnMethod: methods first ].
	methods isEmpty
		ifTrue:[ ^ MethodWidget removeAllFromMethodsIconsCache: aMethod ].
	methodsNames := methods collect: [:each | each methodClass name, '>>#', each selector ].
	index := UIManager default chooseFrom: methodsNames.
	index = 0 ifTrue: [ ^ self ].
	^ Nautilus openOnMethod: (methods at: index)
	
]

{ #category : #'icons behavior' }
AbstractNautilusUI class >> arrowUp: aMethod [
	| newMethod |
	newMethod := aMethod methodClass superclass lookupSelector: aMethod selector.
	newMethod
		ifNil: [ MethodWidget removeAllFromMethodsIconsCache:  aMethod ]
		ifNotNil: [ Nautilus openOnMethod: newMethod ]
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildAddShortcutsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #newClass)
		category: #NautilusGlobalShortcuts
		default: $n meta , $c meta
		do: [ :target | target addClass ]
		description: 'Create a new class'.
	(aBuilder shortcut: #newPackage)
		category: #NautilusGlobalShortcuts
		default: $n meta , $p meta
		do: [ :target | target addPackage ]
		description: 'Create a new package'.
	(aBuilder shortcut: #newProtocol)
		category: #NautilusGlobalShortcuts
		default: $n meta , $t meta
		do: [ :target | target addCategory ]
		description: 'Create a new protocol'.
	(aBuilder shortcut: #newGroup)
		category: #NautilusGlobalShortcuts
		default: $n meta , $g meta
		do: [ :target | target addNewGroup ]
		description: 'Create a new group'
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildBrowseShortcutsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #browseSenders)
		category: #NautilusGlobalShortcuts
		default: $b meta , $n meta
		do: [ :target | target browseSendersOfMessages ]
		description: 'Browse senders of the selected method'.
	(aBuilder shortcut: #browseClassRefs)
		category: #NautilusGlobalShortcuts
		default: $b meta , $n meta shift
		do: [ :target | target browseClassRefs ]
		description: 'Browse class references'.
	(aBuilder shortcut: #browseImplementors)
		category: #NautilusGlobalShortcuts
		default: $b meta , $m meta
		do: [ :target | target browseMessages ]
		description: 'Browse implementors of the selected method'.
	(aBuilder shortcut: #browseRestricted)
		category: #NautilusGlobalShortcuts
		default: $b meta , $s meta
		do: [ :target | target restrictedBrowse ]
		description: 'Open a restricted browser'.
	(aBuilder shortcut: #browseRestrictedClass)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $c meta
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser on the selected class'.
	(aBuilder shortcut: #browseSuperclass)
		category: #NautilusGlobalShortcuts
		default: $b meta , $s meta shift
		do: [ :target | target browseSuperclass ]
		description: 'Open a browser on the superclass of the selected class'.
	(aBuilder shortcut: #browseRestrictedPackage)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $p meta
		do: [ :target | target restrictedBrowsePackage ]
		description: 'Open a restricted browser on the selected package'.
	(aBuilder shortcut: #browseRestrictedRegex)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $r meta
		do: [ :target | target restrictedBrowsePackageRegex ]
		description: 'Open a restricted browser on a package regex'.
	(aBuilder shortcut: #browseRestrictedSuperclasses)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $S meta shift
		do: [ :target | target restrictedBrowseSuperclasses ]
		description: 'Open a restricted browser the superclasses'.
	(aBuilder shortcut: #browseRestrictedSubclasses)
		category: #NautilusGlobalShortcuts
		default: $b meta , $r meta , $s meta
		do: [ :target | target restrictedBrowseSubclasses ]
		description: 'Open a restricted browser the subclasses'.
	(aBuilder shortcut: #browseInheritance)
		category: #NautilusGlobalShortcuts
		default: $b meta , $i meta
		do: [ :target | target methodHierarchy ]
		description: 'Open a restricted browser'.
	(aBuilder shortcut: #browseFull)
		category: #NautilusGlobalShortcuts
		default: $b meta , $f meta
		do: [ :target | target fullBrowse ]
		description: 'Open the same browser'.
	(aBuilder shortcut: #browseVersion)
		category: #NautilusGlobalShortcuts
		default: $m meta , $v meta
		do: [ :target | target browseVersions ]
		description: 'Browse senders of the selected method'.
	(aBuilder shortcut: #browseVariables)
		category: #NautilusGlobalShortcuts
		default: $b meta , $v meta
		do: [ :target | target browseVariableReferences ]
		description: 'Browse variables of the selected class'
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildButtonsShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #showInstanceSide)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$i meta 
		do: [ :target | target showInstance: true ]
		description: 'Show instance side'.

	(aBuilder shortcut: #showClassSide)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$c meta 
		do: [ :target | target showInstance: false ]
		description: 'Show class side'.

	(aBuilder shortcut: #showHierarchy)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$h meta 
		do: [ :target | target showHierarchy: true ]
		description: 'Show Hierarchy'.
		
	(aBuilder shortcut: #showFlat)
		category: #NautilusGlobalShortcuts
		default: $t meta  ,$f meta 
		do: [ :target | target showHierarchy: false ]
		description: 'Show normal view'.

]

{ #category : #'shortcut-old' }
AbstractNautilusUI class >> buildClassShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusClassShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #restrictedBrowseClass)
		category: #NautilusClassShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseClass ]
		description: 'Open a restricted browser'.

	(aBuilder shortcut: #copyClasses)
		category: #NautilusClassShortcuts
		default: $c meta
		do: [ :target | target copyClasses ]
		description: 'Copy the selected classes'.
	
	(aBuilder shortcut: #findMethod)
		category: #NautilusClassShortcuts
		default: $f meta
		do: [ :target | target findMethod ]
		description: 'Find a method'.
		
	(aBuilder shortcut: #findClass)
		category: #NautilusClassShortcuts
		default: $f meta shift
		do: [ :target | target findClass ]
		description: 'Find a class'.
		
	(aBuilder shortcut: #generateInitialize)
		category: #NautilusClassShortcuts
		default: $i meta shift
		do: [ :target | target generateInitialize ]
		description: 'Generate the initialize method'.
		
	(aBuilder shortcut: #createTestForSelectedClass)
		category: #NautilusClassShortcuts
		default: $j meta
		do: [ :target | target enableSingleClassSelection ifTrue: [ target createTestForSelectedClass ]]
		description: 'Generate a test class for the selected class'.
		
	(aBuilder shortcut: #forceGenerateInitialize)
		category: #NautilusClassShortcuts
		default: $k meta shift
		do: [ :target | target forceGenerateInitialize ]
		description: 'Regenerate the initialize method'.
		
	(aBuilder shortcut: #browseClassRefs)
		category: #NautilusClassShortcuts
		default: $n meta shift
		do: [ :target | target browseClassRefs ]
		description: 'Browse class references'.
	
	(aBuilder shortcut: #addClass)
		category: #NautilusClassShortcuts
		default: $n meta
		do: [ :target | target addClass ]
		description: 'Add a class'.
		
	(aBuilder shortcut: #renameClass)
		category: #NautilusClassShortcuts
		default: $r meta
		do: [ :target | target enableSingleClassSelection ifTrue: [ target renameClass ]]
		description: 'Rename the selected class'.
		
	(aBuilder shortcut: #runClassTests)
		category: #NautilusClassShortcuts
		default: $t meta
		do: [ :target | target runClassTests ]
		description: 'Run the tests for the selected class'.
		
	(aBuilder shortcut: #removeClasses)
		category: #NautilusClassShortcuts
		default: $x meta
		do: [ :target | target removeClasses ]
		description: 'Remove the selected classes'
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildCopyShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #copyClass)
		category: #NautilusGlobalShortcuts
		default: $c meta , $c meta 
		do: [ :target | target copyClasses ]
		description: 'Copy the selected classes'.
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildExtraShortcutsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #openBrowser)
		category: #NautilusGlobalShortcuts
		default: $o meta , $o meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser'.
	(aBuilder shortcut: #openBrowserOnSubclass)
		category: #NautilusGlobalShortcuts
		default: $o meta , $s meta
		do: [ :target | target openSubclass ]
		description: 'Open a new browser on a subclass'.
	(aBuilder shortcut: #openBrowserOnSuperclass)
		category: #NautilusGlobalShortcuts
		default: $o meta , $s meta shift
		do: [ :target | target openSuperclass ]
		description: 'Open a new browser on a superclass'.
	(aBuilder shortcut: #toggleBytecodes)
		category: #NautilusGlobalShortcuts
		default: $b meta , $b meta
		do: [ :target | target toggleBytecodes ]
		description: 'ToggleBytecodes'
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildFindShortcutsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #findClass)
		category: #NautilusGlobalShortcuts
		default: $f meta , $c meta
		do: [ :target | target findClass ]
		description: 'Find a class'.
	(aBuilder shortcut: #findClassInHierarchy)
		category: #NautilusGlobalShortcuts
		default: $f meta , $h meta
		do: [ :target | target findClassInHierarchy ]
		description: 'Find a class in the hierarchy'.
	(aBuilder shortcut: #findSuperclass)
		category: #NautilusGlobalShortcuts
		default: $f meta , $S meta shift
		do: [ :target | target findSuperclass ]
		description: 'Find a superclass'.
	(aBuilder shortcut: #findSubclass)
		category: #NautilusGlobalShortcuts
		default: $f meta , $s meta
		do: [ :target | target findSubclass ]
		description: 'Find a subclass'.
	(aBuilder shortcut: #findPackage)
		category: #NautilusGlobalShortcuts
		default: $f meta , $p meta
		do: [ :target | target findPackage ]
		description: 'Find a package'.
	(aBuilder shortcut: #findProtocol)
		category: #NautilusGlobalShortcuts
		default: $f meta , $t meta
		do: [ :target | target findProtocol ]
		description: 'Find a protocol'.
	(aBuilder shortcut: #findMethod)
		category: #NautilusGlobalShortcuts
		default: $f meta , $m meta
		do: [ :target | target findMethod ]
		description: 'Find a method'
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildGotoNavigationShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #jumpToClass)
		category: #NautilusGlobalShortcuts
		default: $g meta , $c meta 
		do: [ :target | target giveFocusToClass ]
		description: 'Jump to the selected class'.
			
	(aBuilder shortcut: #jumpToPackage)
		category: #NautilusGlobalShortcuts
		default: $g meta, $p meta 
		do: [ :target | target giveFocusToPackage ]
		description: 'Jump to the selected package'.

	(aBuilder shortcut: #jumpToProtocol)
		category: #NautilusGlobalShortcuts
		default: $g meta , $t meta 
		do: [ :target | target giveFocusToProtocol ]
		description: 'Jump to the selected protocol'.
		
	(aBuilder shortcut: #jumpToMethod)
		category: #NautilusGlobalShortcuts
		default: $g meta , $m meta  
		do: [ :target | target giveFocusToMethod ]
		description: 'Jump to the selected method'.
		
	(aBuilder shortcut: #jumpToTestClass)
		category: #NautilusGlobalShortcuts
		default: $g meta , $j meta , $c meta
		do: [ :target | target createTestForSelectedClass ]
		description: 'Jump to test class'.
]

{ #category : #'shortcut-old' }
AbstractNautilusUI class >> buildGroupShortcutsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #fullBrowse)
		category: #NautilusGroupShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the current selection'.
	(aBuilder shortcut: #restrictedBrowseGroup)
		category: #NautilusGroupShortcuts
		default: $b meta shift
		do: [ :target | target restrictedBrowseGroups: target packageWidget selectedGroups ]
		description: 'Open a browser on a restricted view'.
	(aBuilder shortcut: #addNewGroup)
		category: #NautilusGroupShortcuts
		default: $n meta
		do: [ :target | target addNewGroup ]
		description: 'Create a new group'.
	(aBuilder shortcut: #renameGroup)
		category: #NautilusGroupShortcuts
		default: $r meta
		do: [ :target | 
			target enableSingleMenuItems
				ifTrue: [ target renameGroup ] ]
		description: 'Rename the selected group'.
	(aBuilder shortcut: #runTestsOfGroups)
		category: #NautilusGroupShortcuts
		default: $t meta
		do: [ :target | target runTestsOfGroups: target packageWidget selectedGroups notifying: true ]
		description: 'Run the tests of the selected groups'.
	(aBuilder shortcut: #removeGroups)
		category: #NautilusGroupShortcuts
		default: $x meta
		do: [ :target | target removeGroups ]
		description: 'Remove the selected groups'
]

{ #category : #'shortcut-old' }
AbstractNautilusUI class >> buildPackageShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #fullBrowse)
		category: #NautilusPackageShortcuts
		default: $b meta
		do: [ :target | target fullBrowse ]
		description: 'Open a new browser on the selection'.
		
	(aBuilder shortcut: #findClass)
		category: #NautilusPackageShortcuts
		default: $f meta
		do: [ :target | target findClass ]
		description: 'Find a class'.
		
	(aBuilder shortcut: #findPackage)
		category: #NautilusPackageShortcuts
		default: $f meta shift
		do: [ :target | target findPackage ]
		description: 'Find a package'.
		
	(aBuilder shortcut: #addPackage)
		category: #NautilusPackageShortcuts
		default: $n meta 
		do: [ :target | target addPackage ]
		description: 'Add a new package'.
	
	(aBuilder shortcut: #renamePackage)
		category: #NautilusPackageShortcuts
		default: $r meta 
		do: [ :target | target enableSingleMenuItems ifTrue: [ target renamePackage ]]
		description: 'Rename the selected package'.	
	
	(aBuilder shortcut: #runPackagesTests)
		category: #NautilusPackageShortcuts
		default: $t meta 
		do: [ :target | target runPackagesTestsNotifying: true ]
		description: 'Run the tests for the selected packages'.
	
	(aBuilder shortcut: #removePackages)
		category: #NautilusPackageShortcuts
		default: $x meta 
		do: [ :target | target removePackages ]
		description: 'Remove the selected packages'.
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildRecategorizeShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #recategorizeMethod)
		category: #NautilusGlobalShortcuts
		default: $m meta , $m meta 
		do: [ :target | target categorizeMethod ]
		description: 'Categorize the selected method'.
		
	(aBuilder shortcut: #recategorizeClass)
		category: #NautilusGlobalShortcuts
		default: $m meta , $c meta 
		do: [ :target | target moveInPackage ]
		description: 'Categorize the selected class'.
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildRefactoringShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #generateInitialize)
		category: #NautilusGlobalShortcuts
		default: $h meta, $i meta 
		do: [ :target | target generateInitialize ]
		description: 'Generate the initialize method'.
		
	(aBuilder shortcut: #forceGenerateInitialize)
		category: #NautilusGlobalShortcuts
		default: $h meta, $k meta
		do: [ :target | target forceGenerateInitialize ]
		description: 'Regenerate the initialize method'.
		
	(aBuilder shortcut: #createTestForSelectedClass)
		category: #NautilusGlobalShortcuts
		default: $n meta, $j meta, $c meta
		do: [ :target | target enableSingleClassSelection ifTrue: [ target createTestForSelectedClass ]]
		description: 'Generate a test class for the selected class'.
		
	(aBuilder shortcut: #generateTestAndJump)
		category: #NautilusGlobalShortcuts
		default: $h meta, $j meta
		do: [ :target | target generateTestMethodsAndFocus: true ]
		description: 'Generate test and jump'.
		
	(aBuilder shortcut: #generateTest)
		category: #NautilusGlobalShortcuts
		default: $h meta, $j meta shift
		do: [ :target | target generateTestMethodsAndFocus: false ]
		description: 'Generate test'.
		
	(aBuilder shortcut: #categorizeAllUncategorized)
		category: #NautilusGlobalShortcuts
		default: $h meta, $c meta shift
		do: [ :target | target categorizeAllUncategorizedMethods ]
		description: 'Categorize all uncategorized and remove empty'.
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildRemoveShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #removeClass)
		category: #NautilusGlobalShortcuts
		default: $x meta  ,$c meta 
		do: [ :target | target removeClassWithRefactoringEngine: target selectedClasses asOrderedCollection ]
		description: 'Remove the selected classes'.
			
	(aBuilder shortcut: #removePackage)
		category: #NautilusGlobalShortcuts
		default: $x meta  , $p meta 
		do: [ :target | target removePackages ]
		description: 'Remove the selected packages'.

	(aBuilder shortcut: #removeProtocol)
		category: #NautilusGlobalShortcuts
		default: $x meta , $t meta 
		do: [ :target | target removeCategories ]
		description: 'Remove the selected protocols'.
		
	(aBuilder shortcut: #removeMethod)
		category: #NautilusGlobalShortcuts
		default: $x meta , $m meta 
		do: [ :target | target removeMethods ]
		description: 'Remove the selected methods'.
		
	(aBuilder shortcut: #removeGroups)
		category: #NautilusGlobalShortcuts
		default: $x meta , $g meta 
		do: [ :target | target removeGroups ]
		description: 'Remove the selected groups'.
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildRenameShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #renameClass)
		category: #NautilusGlobalShortcuts
		default: $r meta  ,$c meta 
		do: [ :target | target renameClass ]
		description: 'Rename the selected class'.
			
	(aBuilder shortcut: #renamePackage)
		category: #NautilusGlobalShortcuts
		default: $r meta  , $p meta 
		do: [ :target | target renamePackage ]
		description: 'Rename the selected package'.

	(aBuilder shortcut: #renameProtocol)
		category: #NautilusGlobalShortcuts
		default: $r meta , $t meta 
		do: [ :target | target renameCategory ]
		description: 'Rename the selected protocol'.
		
	(aBuilder shortcut: #renameGroup)
		category: #NautilusGlobalShortcuts
		default: $r meta , $g meta 
		do: [ :target | target renameGroup ]
		description: 'Rename the selected group'.
]

{ #category : #shortcuts }
AbstractNautilusUI class >> buildRunTestKeymappingsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #runTestForClasses)
		category: #NautilusGlobalShortcuts
		default: $j meta , $c meta
		do: [ :target | target runClassTests ]
		description: 'runTestFor the selected classes'.
	(aBuilder shortcut: #runTestForPackages)
		category: #NautilusGlobalShortcuts
		default: $j meta , $p meta
		do: [ :target | target runPackagesTestsNotifying: true ]
		description: 'runTestFor the selected packages'.
	(aBuilder shortcut: #runTestForMethods)
		category: #NautilusGlobalShortcuts
		default: $j meta , $m meta
		do: [ :target | target runTestForMethods: target selectedMethods notifying: true ]
		description: 'runTestFor the selected methods'
]

{ #category : #menu }
AbstractNautilusUI class >> classesFixMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalClassFixMenu>
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Find Method...')
		keyText: 'f, m' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [target findMethod];
		order: 100;
		help: 'Search for a method by selector'.
		
	(aBuilder item: #'Find Class...')
		keyText: 'f, c' if: Nautilus useOldStyleKeys not;
		keyText: 'F' if: Nautilus useOldStyleKeys;
		action: [ target findClass ];
		order: 200;
		help: 'Search for a method by selector'.
	
	(aBuilder item: #'Add Class...')
		keyText: 'n, c' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [ target addClass ];
		order: 400;
		help: 'Create a new class'.
	(aBuilder item: #'Add Trait...')
		action: [ target addTrait ];
		order: 500;
		withSeparatorAfter.
		
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [target fullBrowse];
		order: 999;
		help: 'Open the same browser'.
]

{ #category : #menu }
AbstractNautilusUI class >> classesMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalClassMenu>
	| target |
	target := aBuilder model.
	target selectedClass ifNil: [ ^ target ].
	(aBuilder item: #'Rename...')
		keyText: 'r,c' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;
		action: [ target renameClass ];
		enabledBlock: [ target selectedClasses size <= 1 ];
		order: -99;
		withSeparatorAfter.
	(aBuilder item: #'Class refs...')
		keyText: 'b, N' if: Nautilus useOldStyleKeys not;
		keyText: 'N' if: Nautilus useOldStyleKeys;
		action: [ target browseClassRefs ];
		order: -99.
	(aBuilder item: #'Browse scoped')
		keyText: 'b, s' if: Nautilus useOldStyleKeys not;
		keyText: 'B' if: Nautilus useOldStyleKeys;
		action: [ target restrictedBrowseClass ];
		order: 1000;
		help: 'Open a browser on a restricted view'.
	target selectedClass isTrait
		ifTrue: [ (aBuilder item: #'Browse users')
				action: [ target restrictedBrowseTraitUsers ];
				order: 1050 ].
	(target selectedClasses
		allSatisfy: [ :each | each isTestCase and: [ each isAbstract not ] ])
		ifTrue: [ (aBuilder item: #'Run tests')
				keyText: 'j, c' if: Nautilus useOldStyleKeys not;
				keyText: 't' if: Nautilus useOldStyleKeys;
				action: [ target runClassTests ];
				order: 1200 ].
	(aBuilder item: #Analyze)
		enabledBlock: [ target selectedClasses size <= 1 ];
		order: 0;
		withSeparatorAfter.
	(aBuilder item: #'Jump to test class')
		keyText: 'g, j' if: Nautilus useOldStyleKeys not;
		keyText: 'j' if: Nautilus useOldStyleKeys;
		action: [ target createTestForSelectedClass ];
		enabledBlock: [ target selectedClasses size = 1 ];
		order: 300;
		withSeparatorAfter.
	(aBuilder item: #'Move to package...')
		keyText: 'm, c' if: Nautilus useOldStyleKeys not;
		action: [ target moveInPackage ];
		order: 1400.
	(aBuilder item: #'Copy...')
		keyText: 'c, c' if: Nautilus useOldStyleKeys not;
		keyText: 'c' if: Nautilus useOldStyleKeys;
		action: [ target copyClasses ];
		order: 1600.
	(aBuilder item: #'Remove...')
		keyText: 'x, c' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target
				removeClassWithRefactoringEngine: target selectedClasses asOrderedCollection ];
		iconName: #removeIcon;
		order: 1700;
		withSeparatorAfter.
	(aBuilder item: #'Inspect Methods')
		action: [ target inspectMethods ];
		withSeparatorAfter;
		order: 1750.
	(aBuilder item: #'File Out')
		action: [ target fileOutClass ];
		withSeparatorAfter;
		order: 1800
]

{ #category : #menu }
AbstractNautilusUI class >> groupsFixMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalGroupFixMenu>
	| target |
	target := aBuilder model.
	(aBuilder item: #'Find Class...')
		keyText: 'f, c' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [target findClass];
		order: 0;
		help: 'Search for a class by name'.

	(aBuilder item: #'New group...')
		keyText: 'n, g' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [target addNewGroup];
		order: 100;
		withSeparatorAfter.
	
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [target fullBrowse];
		order: 999.
]

{ #category : #menu }
AbstractNautilusUI class >> groupsMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalGroupMenu>
	| target |
	target := aBuilder model.
	target selectedGroup ifNil: [ ^ target ].
	(aBuilder item: #'Browse scoped')
		keyText: 'b, s' if: Nautilus useOldStyleKeys not;
		action: [ target restrictedBrowseGroups: target packageWidget selectedGroups ];
		order: 1000;
		withSeparatorAfter.
	(aBuilder item: #'Run tests')
		keyText: 'j, g' if: Nautilus useOldStyleKeys not;
		keyText: 't' if: Nautilus useOldStyleKeys;
		action: [ target runTestsOfGroups: target packageWidget selectedGroups notifying: true ];
		order: 1100.
	target selectedGroup
		ifNotNil: [ 
				(aBuilder item: #'Open group manager')
						action: [ target openGroupManager ];
						order: 1150.
			target selectedGroup isReadOnly
				ifFalse: [ 
					(aBuilder item: #'Rename...')
						keyText: 'r, g' if: Nautilus useOldStyleKeys not;
						keyText: 'r' if: Nautilus useOldStyleKeys;
						action: [ target renameGroup ];
						enabledBlock: [ target packageWidget selectedGroups size <= 1 ];
						order: 1200 ].
			target selectedGroup removable
				ifTrue: [ 
					(aBuilder item: #'Empty the group...')
						action: [ target selectedGroup removeAllPackages ];
						order: 1300.
					(aBuilder item: #'Remove...')
						keyText: 'x, g' if: Nautilus useOldStyleKeys not;
						keyText: 'x' if: Nautilus useOldStyleKeys;
						iconName: #removeIcon;
						action: [ target removeGroups ];
						order: 1400 ] ]
]

{ #category : #icons }
AbstractNautilusUI class >> icon [
	^ self iconNamed: #nautilusIcon 
]

{ #category : #'class initialization' }
AbstractNautilusUI class >> initialize [
	" self initialize "
	PackagesIconsCache := WeakIdentityKeyDictionary new.
	
	NextFocusKey := Character arrowRight.
	PreviousFocusKey := Character arrowLeft.
	
	self registerSystemAnnouncements.
]

{ #category : #accessing }
AbstractNautilusUI class >> nextFocusKey [

	^ NextFocusKey
]

{ #category : #'instance creation' }
AbstractNautilusUI class >> on: aNautilus [

	^ self new model: aNautilus
]

{ #category : #'menu pragmas' }
AbstractNautilusUI class >> packageFixPragma [
	^ 'nautilusGlobalPackageFixMenu'
]

{ #category : #menu }
AbstractNautilusUI class >> packagesFixMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalPackageFixMenu>
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Find Class...')
		keyText: 'f, c' if: Nautilus useOldStyleKeys not;
		keyText: 'f' if: Nautilus useOldStyleKeys;
		action: [target findClass];
		order: 0;
		help: 'Search for a class by name'.
		
	(aBuilder item: #'Find Package...')
		keyText: 'f, p' if: Nautilus useOldStyleKeys not;
		keyText: 'F' if: Nautilus useOldStyleKeys;
		action: [ target findPackage ];
		order: 100;
		help: 'Search for a package by name';
		withSeparatorAfter.
		
	(aBuilder item: #'Add package...')
		keyText: 'n, p' if: Nautilus useOldStyleKeys not;
		keyText: 'n' if: Nautilus useOldStyleKeys;
		action: [target addPackage];
		order: 200;
		help: 'Add a package';
		withSeparatorAfter.
		
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [target fullBrowse];
		order: 999;
		help: 'Open the same browser'.
]

{ #category : #accessing }
AbstractNautilusUI class >> packagesIconsCache [

		^ PackagesIconsCache
]

{ #category : #accessing }
AbstractNautilusUI class >> packagesIconsCache: anObject [
	
		PackagesIconsCache := anObject
]

{ #category : #menu }
AbstractNautilusUI class >> packagesMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalPackageMenu>
	| package target |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ target ].
	(aBuilder item: #'Browse scoped')
		keyText: 'b, s' if: Nautilus useOldStyleKeys not;
		action: [ target restrictedBrowsePackage ];
		order: 1000;
		help: 'Open a browser on a restricted view';
		withSeparatorAfter.
	(aBuilder item: #'Rename...')
		keyText: 'r, p' if: Nautilus useOldStyleKeys not;
		keyText: 'r' if: Nautilus useOldStyleKeys;
		action: [ target renamePackage ];
		order: 1100;
		help: 'Rename a package';
		enabledBlock: [ target selectedPackages size < 2 ].
	(aBuilder item: #'Remove...')
		keyText: 'x, p' if: Nautilus useOldStyleKeys not;
		keyText: 'x' if: Nautilus useOldStyleKeys;
		action: [ target removePackages ];
		order: 1200;
		help: 'Remove a package';
		iconName: #removeIcon.
	(aBuilder item: #'Run tests...')
		keyText: 'j, p' if: Nautilus useOldStyleKeys not;
		keyText: 't' if: Nautilus useOldStyleKeys;
		action: [ target runPackagesTestsNotifying: true ];
		enabled: (package classes anySatisfy: [ :e | e isTestCase ]);
		order: 1250;
		help: 'Run tests';
		withSeparatorAfter.
	(aBuilder item: #'Promote as package')
		action: [ target promoteSelectedPackageTagAsPackage ];
		enabled: package isPackageTag;
		order: 1260;
		help: 'Promote this package tag as one package'.
	(aBuilder item: #'Demote to package with tag')
		action: [ target demoteSelectedPackageAsPackageWithTag ];
		enabled: (package isPackageTag not and: [ package name includes: $- ]);
		order: 1261;
		help: 'Downgrade this package as one package with a tag';
		withSeparatorAfter.
	(aBuilder item: #'File Out')
		action: [ target fileOutPackage ];
		order: 1300;
		help: 'File out a package';
		withSeparatorAfter.
	self packagesMenuGroupsItems: aBuilder
]

{ #category : #menu }
AbstractNautilusUI class >> packagesMenuGroupsItems:aBuilder [
	| target order |
	target := aBuilder model.
	order := 0.
	(aBuilder item: #'Add in Group...')
		order: 1600;
		help: 'Add current package in a group';
		enabledBlock:[target selectedPackage isPackageTag not].
	target groupsManager groups do: [ :group |  
		(aBuilder item: group name asSymbol)
			action: [ target addPackages: target selectedPackages inGroup: group ];
			parent: #'Add in Group...';
			order: order.
			order := order + 10
		].
	target parentOfSelectedPackage ifNil: [ ^ self ].
	(aBuilder item: #'Remove from Group')
		action: [ target removeFromGroupPackages: target selectedPackages ];
		order: 1650;
		help: 'Remove selected packages from group';
		withSeparatorAfter;
		enabledBlock:[target selectedPackage isPackageTag not & target parentOfSelectedPackage isNotNil].

]

{ #category : #accessing }
AbstractNautilusUI class >> previousFocusKey [

	^ PreviousFocusKey
]

{ #category : #'class initialization' }
AbstractNautilusUI class >> registerSystemAnnouncements [
	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak
		when: IconSetChanged
		send: #resetIconCaches
		to: self
]

{ #category : #icons }
AbstractNautilusUI class >> resetIconCaches [
	PackagesIconsCache removeAll.
	MethodWidget resetMethodIconCache.
]

{ #category : #'icons behavior' }
AbstractNautilusUI class >> runClassInitializeMethod: aMethod [
	"Perform the method as an example method"
 	|selectedClass|
	(UIManager default confirm: 'Would you like to (re)initialize the class')
		ifFalse: [ ^self ].
	selectedClass := aMethod methodClass baseClass.	
	selectedClass perform: aMethod selector.
	UIManager default inform: selectedClass name asString, ' initialized'.
	 
]

{ #category : #'icons behavior' }
AbstractNautilusUI class >> runExamplarMethod: aMethod [
	"Perform the method as an example method. Now since this is an examplar (an bare instance of the class) with in addition inspect it so that the user can get a cool inspector to play with it."
 
	(aMethod methodClass baseClass perform: aMethod selector) inspect
]

{ #category : #'icons behavior' }
AbstractNautilusUI class >> runExampleMethod: aMethod [
	"Perform the method as an example method"
 
	aMethod methodClass baseClass perform: aMethod selector
]

{ #category : #'icons behavior' }
AbstractNautilusUI class >> saveDirtyPackages: aCollection [ 
	
	
	aCollection isEmptyOrNil ifTrue: [ ^ self ].
	aCollection do: [:package || workCopy browser |
		
		workCopy := MCWorkingCopy forPackage: (MCPackage named: package name).
		browser :=MCWorkingCopyBrowser new.
		browser
			show;
			workingCopy: workCopy.
		"wrap := MCDependentsWrapper with: workCopy model: browser.
		self halt.
		index := browser workingCopySelectionWrapper: wrap.
		browser workingCopyTreeMorph selectionIndex: index"]
	
]

{ #category : #icons }
AbstractNautilusUI class >> taskbarIconName [
	^#nautilusIcon 
]

{ #category : #'group announcements' }
AbstractNautilusUI >> aGroupContentHasBeenModified: anAnnouncement [
	self updateGroupView
]

{ #category : #'group announcements' }
AbstractNautilusUI >> aGroupHasBeenAdded: anAnnouncement [
	"(NautilusUI methodDict at: #aGroupHasBeenRegistered:) getSource"

	| group |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	group := anAnnouncement group.
	(self groupsManager includes: group)
		ifTrue: [ self updateGroupView ]
]

{ #category : #'group announcements' }
AbstractNautilusUI >> aGroupHasBeenRemoved: anAnnouncement [
	 "(NautilusUI methodDict at: #aGroupHasBeenRegistered:) getSource"

	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [^ self ].
	(self groupsManager =  anAnnouncement holder)
		ifTrue: [ 
			self selectedGroup = anAnnouncement group
				ifTrue: [ self selectedGroup: nil ].
			self updateGroupView.
			self updateCategoryAndMethodList ]
]

{ #category : #'group announcements' }
AbstractNautilusUI >> aGroupHasBeenRenamed: anAnnouncement [
	"(NautilusUI methodDict at: #aGroupHasBeenRegistered:) getSource"

	self aGroupHasBeenAdded: anAnnouncement 
]

{ #category : #'comment pane' }
AbstractNautilusUI >> acceptCommentFromView [
	self commentTextModel acceptEditsInView
]

{ #category : #'source code area' }
AbstractNautilusUI >> acceptSourceCodeFromView [

	self sourceTextModel acceptEditsInView
]

{ #category : #'build ui' }
AbstractNautilusUI >> addAll: aWindow [
	| topHeight middleHeight bottomMorph bottomHeight delta textSegmentIcons |
	delta := 0.
	bottomHeight := 0.
	middleHeight := 0.
	topHeight := 0.
	sourceTextWidget := self buildCodePane.
	sourceTextWidget announcer when: RubTextAccepted send: #whenTextAcceptedInView: to: self.
	
	self buildTopPlugins
		ifNotNil:
			[ :top | 
			topHeight := top height.
			aWindow
				addMorph: top
				fullFrame: (LayoutFrame identity bottomFraction: 0;  bottomOffset: topHeight).
			delta := 4.
			topHeight := topHeight + delta.
			top color: Color transparent ].
	self buildTopBarOn: aWindow height: topHeight.
	topHeight := topHeight + self buttonsBarHeight + 4.
	self buildColumns: aWindow height: topHeight.
	self setShortcuts: #NautilusSourceCodeShortcuts to: aWindow.
	self buildMiddlePlugins
		ifNotNil:
			[ :middle | 
			middleHeight := middle height + 4.
			aWindow
				addMorph: middle
				fullFrame:
					(LayoutFrame identity
						topFraction: self navigationAreaProportion;
						bottomFraction: self navigationAreaProportion; 
						bottomOffset: middleHeight).
			middle color: Color transparent ].
	sourceCodePanel := PanelMorph new.
	self setShortcuts: #NautilusSourceCodeShortcuts to: sourceCodePanel.
	sourceCodePanel changeProportionalLayout.
	sourceCodePanel
		addMorph: sourceTextWidget
		fullFrame: LayoutFrame identity.
	sourceCodePanel
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	sourceCodeContainer := PanelMorph new.
	self
		setShortcuts: #NautilusSourceCodeShortcuts
		to: sourceCodeContainer.
	sourceCodeContainer
		changeTableLayout;
		listDirection: #leftToRight.
	sourceCodeContainer addMorph: sourceCodePanel.
	bottomMorph := self buildBottomPlugins.
	bottomMorph ifNotNil: [ bottomHeight := bottomMorph height + 8 ].
	aWindow
		addMorph: sourceCodeContainer
		fullFrame:
			(LayoutFrame identity
				topFraction: self navigationAreaProportion;
				topOffset: middleHeight;
				bottomOffset: bottomHeight negated).
	bottomMorph
		ifNotNil:
			[ 
			aWindow
				addMorph: bottomMorph
				fullFrame:
					(LayoutFrame identity 
						topFraction: 1;
						topOffset: bottomHeight negated).
			bottomMorph color: Color transparent ].
		
	textSegmentIcons := sourceTextWidget rulerNamed: #textSegmentIcons.
	textSegmentIcons on: #click send: #whenTextSegmentIconsClicked: to: self.
	textSegmentIcons on: #mouseUp send: #whenTextSegmentIconsMouseUp: to: self.

]

{ #category : #'menus behavior' }
AbstractNautilusUI >> addCategory [
	^ self subclassResponsibility
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> addClass [
	| package |
	package := self selectedPackage 
		ifNil: [ RPackageOrganizer default packageNamed: RPackage defaultPackageName ].
	self addClassIn: package.
]

{ #category : #styling }
AbstractNautilusUI >> addIconStyle [

	self selectedMethod ifNil: [ ^self ].
	IconStyler styleText: self sourceTextModel withAst: self selectedMethod ast.
	Nautilus inlineCritiques ifTrue: [ 
		self selectedMethod annotateRubricText: self sourceTextModel ]
]

{ #category : #menus }
AbstractNautilusUI >> addModelItemsToWindowMenu: aMenu [
	"Add model-related items to the window menu"

	aMenu addLine;
		add: 'Nautilus Plugins Manager'
		target: NautilusPluginManager new
		selector: #openInWorld.
		
	aMenu
		add: 'Shortcuts description'
		target: self
		selector: #openShortcutDescription.
]

{ #category : #group }
AbstractNautilusUI >> addNewGroup [
	| group |
	[ group := self groupsManager createAnEmptyStaticGroup ]
		on: GroupAlreadyExists
		do: [ :ex | self alertGroupExisting: ex groupName ].
	GroupAnnouncer uniqueInstance announce: (AGroupHasBeenAdded group: group into: self)
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> addPackage [

	| package |
	package := self addPackageBasedOn: self selectedPackage.
	package ifNotNil: [
			self selectedPackage: package.
			self updatePackageViewAndMove ]
]

{ #category : #group }
AbstractNautilusUI >> addPackages: aCollection inGroup: aGroup [
	aGroup addPackages: aCollection
]

{ #category : #private }
AbstractNautilusUI >> addSubclassesOf: aClass in: result withIndex: index [

	| classes |
	classes := aClass subclasses "self selectedPackage
					ifNil: [ {} ]
					ifNotNil: [:p | aClass subclasses intersection: p classes ]".

	classes do: [:each |
		result at: each put: index.
		self addSubclassesOf: each theNonMetaClass in: result withIndex: index + 1 ]
]

{ #category : #private }
AbstractNautilusUI >> addSubclassesOf: aClass in: result withIndex: index fromRestrictedCollection: aCollection [

	| classes |

	classes := aClass subclasses" intersection: self shownClasses".
	
	classes := classes intersection: aCollection.
	(classes sort: [:a :b| a name <= b name]) do: [:each |
		result at: each put: index.
		self addSubclassesOf: each in: result withIndex: index + 1 fromRestrictedCollection: aCollection ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> addTrait [

	self selectedPackage ifNotNil: [:package | self addTraitIn: package ]
	
	" No need of update, announcements will do the job "
]

{ #category : #group }
AbstractNautilusUI >> alertGroupExisting: name [
	UIManager default alert: 'The group named ' , name , ' already exists' title: 'Already exists'
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> announce: anAnnouncement [

	^ self model announce: anAnnouncement
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> announcer [

	^ self model announcer
]

{ #category : #private }
AbstractNautilusUI >> asYetUnclassifiedString [

	^ Protocol unclassified.
]

{ #category : #dispatch }
AbstractNautilusUI >> basicRenameClass: aClass [

	self renameClass: aClass
]

{ #category : #breakpoints }
AbstractNautilusUI >> breakOn: aNode [

	| aMethod |
	
	aNode hasBreakpoint ifTrue: [ Breakpoint removeFrom: aNode ].
	
	aMethod := self selectedMethod.

	Breakpoint new node: aNode; always; install.
	SystemAnnouncer uniqueInstance methodChangedFrom: aMethod to: aMethod oldProtocol: aMethod protocol.	
]

{ #category : #breakpoints }
AbstractNautilusUI >> breakOnConditionOn: aNode [

	| aMethod |
	
	aNode hasBreakpoint ifTrue: [ Breakpoint removeFrom: aNode ].

	aMethod := self selectedMethod.
	
	(UIManager default request: 'Break on what condition?' translated)	
		ifNotNil: [ :condition | 
			Breakpoint new 
				node: aNode;
				condition: (Smalltalk compiler evaluate: condition);
				install.
				
			SystemAnnouncer uniqueInstance methodChangedFrom: aMethod to: aMethod oldProtocol: aMethod protocol.	 ]
	
]

{ #category : #breakpoints }
AbstractNautilusUI >> breakOnceOn: aNode [

	| aMethod |
	
	aNode hasBreakpoint ifTrue: [ Breakpoint removeFrom: aNode ].

	aMethod := self selectedMethod.

	Breakpoint new node: aNode; once; install.
	SystemAnnouncer uniqueInstance methodChangedFrom: aMethod to: aMethod oldProtocol: aMethod protocol.	
]

{ #category : #breakpoints }
AbstractNautilusUI >> breakpointInstallationNodeFor: aCursorPoint [

	| aMethod anAST aCode aNode startPosition endPosition line lineIndex ruler selection |

	ruler := sourceTextWidget rulerNamed: #textSegmentIcons.
	lineIndex := ruler lineIndexForPoint: aCursorPoint.
	line := ruler paragraph lines at: lineIndex.

	aMethod := self selectedMethod.
	aCode := aMethod sourceCode.
	anAST := aMethod ast.
	
	startPosition := line first.
	endPosition := line last.
	
	selection := self selectionInterval.
	((selection notNil and: [ selection first >= startPosition ]) and: [ selection last <= endPosition ]) 
		ifTrue: [ 
			startPosition := selection first max: 1.
			endPosition := selection last min: aCode size].
		
	aNode := anAST bestNodeFor: (startPosition to: endPosition).
	
	^ aNode ifNil: [ anAST ].
]

{ #category : #'plugins announcements' }
AbstractNautilusUI >> browseClass: aClass [
	self selectedPackage: (self parentOfClass: aClass).
	self selectedClass: aClass.
	self updatePackageGroupAndClassList
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseClassRefs [
	
	self browseClassRefsOf: self selectedClass.
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseClassVarRefs [

	self browseClassVarRefsOf: self selectedClass
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseInstVarDefs [ 

	self browseInstVarDefsOf: self selectedClass
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseInstVarRefs [ 

	self browseInstVarRefsOf: self selectedClass
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseMessages [
	self selectedMethod ifNil: [ ^ self inform: 'No method selected' ].
	self browseMessagesFrom: self selectedMethod selector
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> browseScopedButtonAction [
	self restrictedBrowsePackage
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> browseScopedButtonEnabled [
	^ self selectedPackage notNil
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> browseScopedButtonLabel [
	^ 'Scoped'
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> browseScopedButtonState [
	^ false
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseSendersOfMessages [
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self selectedMethod ifNotNil: [ :met | self browseSendersOfMessagesFrom: met selector ]
]

{ #category : #'plugins announcements' }
AbstractNautilusUI >> browseSuperclass [

	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self browseSuperclassOf: class ]
]

{ #category : #'plugins announcements' }
AbstractNautilusUI >> browseSuperclassOf: class [
	^ self browseClass: class superclass
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseUnusedMethods [
	
	self browseUnusedMethodsOf: self selectedClass
]

{ #category : #'button-variables' }
AbstractNautilusUI >> browseVariableReferences [

	| mainMenu classVarSubMenu |

	(self selectedClass isNil or:[ self selectedClass isTrait]) ifTrue: [ ^self ].
	
	classVarSubMenu := MenuMorph new.
	
	self 
	buildVariableMenuFor: classVarSubMenu 
	variablesFrom: #classVarNames 
	menuItemBy: [ :variable :variableClass :menu |
		
		menu 			 
		add: variable asString
		target: [ 
			self systemNavigation browseAllReferencesTo: (variableClass classPool associationAt: variable) 
		]
		selector: #value.
		
		menu lastItem icon: (self iconNamed: #classIcon)
		
	].
	
	mainMenu := MenuMorph new.
	mainMenu 
	buildTitle: [ :menuTitle | menuTitle
		title: (self selectedClass printString);
		color: Color transparent;
		font: StandardFonts windowTitleFont
	];
	add: 'Class Variables' subMenu: (classVarSubMenu).
	
	self 
	buildVariableMenuFor: mainMenu 
	variablesFrom: #instVarNames 
	menuItemBy: [ :variable :variableClass :menu |
		
		menu 			 
		add: variable asString
		target: [ 
			self systemNavigation browseAllAccessesTo: variable from: self selectedClass
		]
		selector: #value.
		
	].
	
	mainMenu popUpInWorld.

]

{ #category : #'button-variables' }
AbstractNautilusUI >> browseVariablesButtonAction [

	self browseVariableReferences
]

{ #category : #'button-variables' }
AbstractNautilusUI >> browseVariablesButtonEnabled [

	^ self selectedClass notNil
]

{ #category : #'button-variables' }
AbstractNautilusUI >> browseVariablesButtonLabel [

	^ 'Variables'
]

{ #category : #'button-variables' }
AbstractNautilusUI >> browseVariablesButtonState [

	^ false
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> browseVersions [
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	self selectedMethod ifNil: [ ^ self inform: 'No method selected' ].
	self browseVersionsFrom: self selectedMethod
]

{ #category : #private }
AbstractNautilusUI >> browsedEnvironment [

	^ self model browsedEnvironment
]

{ #category : #'build ui plugins' }
AbstractNautilusUI >> buildBottomPlugins [

	| bottom container |
	bottom := self model plugins select: [:each | each position = #bottom ].
	bottom ifEmpty: [ ^ nil ].
	bottom size = 1 ifTrue: [ ^ bottom first display ].
	container := Morph new.
	self setShortcuts: #NautilusSourceCodeShortcuts to: container.
	container 
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #topToBottom;
		cellPositioning: #topLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	bottom reverse do: [:each |
		each display ifNotNil: [:morph | container addMorph: morph ]].
	^ container
]

{ #category : #'build ui buttons' }
AbstractNautilusUI >> buildBrowseScopedButton [
	^ (PluggableButtonMorph
		on: self
		getState: #browseScopedButtonState
		action: #browseScopedButtonAction
		label: #browseScopedButtonLabel)
		getEnabledSelector: #browseScopedButtonEnabled;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		enabled: self selectedPackage notNil;
		helpText: 'Browse the selected packaged in a scoped context: analysis tools such as ''Senders of...'', ''Implementors of...'', ''Class refs...'', etc. are limited to the package';
		yourself
]

{ #category : #'build ui buttons' }
AbstractNautilusUI >> buildBrowseVariablesButton [
	^ (PluggableButtonMorph
		on: self
		getState: #browseVariablesButtonState
		action: #browseVariablesButtonAction
		label: #browseVariablesButtonLabel)
		getEnabledSelector: #browseVariablesButtonEnabled;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		enabled: (self selectedClass notNil and:[ self selectedClass isTrait not]);
		helpText: 'List the instance and class variables of the selected class';
		yourself
]

{ #category : #'build ui' }
AbstractNautilusUI >> buildButtonsBar [
	| arrows buttons |
	arrows := PanelMorph new
		changeProportionalLayout;
		addMorph: self buildNavigationPreviousButton fullFrame: (LayoutFrame identity rightFraction: 0.5);
		addMorph: self buildNavigationNextButton fullFrame: (LayoutFrame identity leftFraction: 0.5);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.
	buttons := PanelMorph new
		changeProportionalLayout;
		addMorph: self buildBrowseScopedButton
			fullFrame: 
			 (LayoutFrame identity rightFraction: 0.1 ;topLeftOffset: 2 @ 0);
		addMorph: self buildBrowseVariablesButton
			fullFrame: 
			 (LayoutFrame identity leftFraction: 0.1 ; rightFraction: 0.2 ; topLeftOffset: 2 @ 0);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: buttons
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0;
				yourself);
		addMorph: self buildNavigationList
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0.5;
				rightFraction: 0.95;
				yourself);
		addMorph: arrows
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0.95;
				yourself);
		yourself
]

{ #category : #'widget category' }
AbstractNautilusUI >> buildCategoryWidget [
	self subclassResponsibility
]

{ #category : #'widget class' }
AbstractNautilusUI >> buildClassWidget [
	self subclassResponsibility 
]

{ #category : #'build ui text' }
AbstractNautilusUI >> buildCodePane [
	^ self buildNewCodeSourceAreaRubric
	
]

{ #category : #'code panel' }
AbstractNautilusUI >> buildCodePanelWithComment [

	commentWidget := self buildNewCommentText.
	self buildCodePanelWithCommentOnRight 
]

{ #category : #'code panel' }
AbstractNautilusUI >> buildCodePanelWithCommentOnRight [
	| splitter delta  |
	
	splitter := ProportionalSplitterMorph new.
	delta := 2.
	splitter addLeftOrTop: sourceTextWidget.
	splitter addRightOrBottom: commentWidget.

	sourceCodePanel
		addMorph: sourceTextWidget
		fullFrame: (LayoutFrame identity rightFraction: 0.5 ; rightOffset: delta negated).
	sourceCodePanel
		addMorph: splitter
		fullFrame: (LayoutFrame identity leftFraction: 0.5; rightFraction: 0.5 ;
						leftOffset: delta negated ; rightOffset: delta).
	sourceCodePanel 
		addMorph: commentWidget
		fullFrame: (LayoutFrame identity leftFraction: 0.5 ;
						leftOffset: delta).
]

{ #category : #'code panel' }
AbstractNautilusUI >> buildCodePanelWithoutComment [
	sourceCodePanel removeAllMorphs.
	self commentTextModel announcer unsubscribe: self.
	commentTextModel := nil.
	sourceCodePanel addMorph: sourceTextWidget fullFrame: LayoutFrame identity
]

{ #category : #'build ui' }
AbstractNautilusUI >> buildColumns: aWindow height: height [
	aWindow
		addMorph: (self buildFirstColumn: aWindow)
		fullFrame:
			(LayoutFrame identity
				rightFraction: 0.22;
				bottomFraction: self navigationAreaProportion;
				topOffset: height).
	aWindow
		addMorph: (self buildSecondColumn: aWindow)
		fullFrame:
			(LayoutFrame identity
				leftFraction: 0.22;
				rightFraction: 0.50;
				bottomFraction: self navigationAreaProportion;
				topOffset: height).
	aWindow
		addMorph: (self buildThirdColumn: aWindow)
		fullFrame:
			(LayoutFrame identity
				leftFraction: 0.50;
				rightFraction: 0.72;
				bottomFraction: self navigationAreaProportion;
				topOffset: height).
	aWindow
		addMorph: (self buildFourthColumn: aWindow)
		fullFrame:
			(LayoutFrame identity
				leftFraction: 0.72;
				bottomFraction: self navigationAreaProportion;
				topOffset: height).
				
		
]

{ #category : #'build ui buttons' }
AbstractNautilusUI >> buildCommentButton [
	^ (PluggableToggleButtonMorph
		on: self
		getState: #commentButtonState
		action: #commentButtonAction:
		label: #commentButtonLabel
		icon: #commentButtonIcon
		menu: nil)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		helpText: 'Show the class comments';
		yourself
]

{ #category : #'build ui lists' }
AbstractNautilusUI >> buildFirstColumn: aWindow [
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: self buildPackageWidget
			fullFrame:
			(LayoutFrame identity bottomOffset: 1 - (StandardFonts defaultFont pixelSize + 10));
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]

{ #category : #'build ui lists' }
AbstractNautilusUI >> buildFourthColumn: aWindow [
		
	^ self buildMethodWidget
]

{ #category : #'build ui buttons' }
AbstractNautilusUI >> buildHierarchyButton [
	^ (PluggableToggleButtonMorph
		on: self
		getState: #hierarchyButtonState
		action: #hierarchyButtonAction:
		label: #hierarchyButtonLabel
		icon: #hierarchyButtonIcon
		menu: nil)
		getEnabledSelector: #hierarchyButtonEnabled;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		enabled: self model selectedClass notNil;
		helpText: 'Show the class hierarchy';
		yourself
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> buildInitializeCodeFor: aClass [ 

	^ String streamContents: [:str | 
		str << 'initialize';cr.
		aClass isMeta ifFalse: [str cr tab << 'super initialize.';cr.].
		aClass instVarNames sorted do: [ :name | 
			str cr tab << name << ' := nil.' ]].
	
]

{ #category : #'build ui buttons' }
AbstractNautilusUI >> buildInstanceButton [
	^ (PluggableToggleButtonMorph
		on: self
		getState: #instanceButtonState
		action: #instanceButtonAction:
		label: #instanceButtonLabel
		icon: #instanceButtonIcon
		menu: nil)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		helpText: (self instanceButtonHelpTextFor: self instanceButtonState);
		yourself
]

{ #category : #'widget method' }
AbstractNautilusUI >> buildMethodWidget [
	self subclassResponsibility
]

{ #category : #'build ui plugins' }
AbstractNautilusUI >> buildMiddlePlugins [

	| middle container |
	middle := self model plugins select: [:each | each position = #middle ].
	middle ifEmpty: [ ^ nil ].
	middle size = 1 ifTrue: [ ^ middle first display ].
	container := Morph new.
	self setShortcuts: #NautilusSourceCodeShortcuts to: container.
	container 
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #topToBottom;
		cellPositioning: #topLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	middle reverse do: [:each |
		each display ifNotNil: [:morph | container addMorph: morph ]].
	^ container
]

{ #category : #'build ui lists' }
AbstractNautilusUI >> buildNavigationList [
	| navigation |
	navigation := (DropListMorph
		on: self
		list: #getHistoryList
		selected: #currentHistoryIndex 
		changeSelected: #setHistory:)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		wrapSelector: #wrapHistory:;
		ghostText: 'History Navigator';
		yourself.
	self setShortcuts: #NautilusSourceCodeShortcuts to: navigation.
	^ navigation
]

{ #category : #'build ui buttons' }
AbstractNautilusUI >> buildNavigationNextButton [
	^ IconicButton new
		target: self;
		actionSelector: #next;
		labelGraphic: (self iconNamed: #forwardIcon);
		color: Color transparent;
		borderWidth: 0;
		helpText: 'Go forward in the navigation history';
		yourself
]

{ #category : #'build ui buttons' }
AbstractNautilusUI >> buildNavigationPreviousButton [
	^ IconicButton new
		target: self;
		actionSelector: #previous;
		labelGraphic: (self iconNamed: #backIcon);
		color: Color transparent;
		borderWidth: 0;
		helpText: 'Go back in the navigation history';
		yourself
]

{ #category : #'build ui text' }
AbstractNautilusUI >> buildNewCodeSourceAreaRubric [
	| scrolledText |
	scrolledText := self sourceTextModel newScrolledText
		beForSmalltalkCode;
		beWrapped;
		yourself.
		
	Nautilus showSourceCodeAnnotation ifTrue: [
		scrolledText withSourceCodeAnnotation].
	
	self updateSourceCodeAreaWith: self sourceCode.
	scrolledText textArea announcer when: RubSelectionChanged send: #whenTextAreaSelectionChanged: to: self.
	self sourceTextModel announcer 
		when: RubTextAcceptRequest send: #whenTextAcceptedInView: to: self;
		when: NautilusClassSelected , NautilusPackageSelected , NautilusGroupSelected send: #showScriptCode: to: self;
		when: NautilusProtocolSelected , NautilusMethodSelected send: #showSourceCode: to: self.
	self setSourceCodeShorcutsTo: scrolledText.
	scrolledText menuProvider: self selector: #sourceCodeMenu:shifted:.
	^ scrolledText
]

{ #category : #'build ui text' }
AbstractNautilusUI >> buildNewCommentText [
	| scrolledText |
	scrolledText := self commentTextModel newScrolledText
		beForSmalltalkComment; 
		withCommentAnnotation;
		beWrapped;
		yourself.
	self commentTextModel setInitialText: self getComments.
	commentTextModel announcer
		when: RubTextAcceptRequest send: #whenTextUpdatedInCommentPane: to: self;
		when: NautilusExtendCommentSwitch send: #whenExtendCommentSwitch: to: self;
		when: RubTextUpdatedInModel send: #whenTextUpdatedInCommentPane: to: self.
	^ scrolledText
]

{ #category : #'widget package' }
AbstractNautilusUI >> buildPackageWidget [
	self subclassResponsibility
]

{ #category : #'build ui lists' }
AbstractNautilusUI >> buildSecondColumn: aWindow [
	| buttons buttonSize |
	buttonSize := StandardFonts defaultFont pixelSize + 10.
	buttons := PanelMorph new.
	buttons
		changeProportionalLayout;
		addMorph: self buildHierarchyButton
			fullFrame:
			(LayoutFrame identity
				rightFraction: 0.33;
				bottomFraction: 0;
				bottomOffset: buttonSize;
				rightOffset: -2).
	instanceButtonWidget := self buildInstanceButton.
	buttons
		addMorph: instanceButtonWidget
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0.33;
				rightFraction: 0.67;
				bottomFraction: 0;
				bottomOffset: buttonSize;
				rightOffset: -2);
		addMorph: self buildCommentButton
			fullFrame:
			(LayoutFrame identity
				leftFraction: 0.67;
				bottomFraction: 0;
				bottomOffset: buttonSize;
				rightOffset: -2);
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: 25.
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: self buildClassWidget
			fullFrame: (LayoutFrame identity bottomOffset: 1 - (buttonSize + 4));
		addMorph: buttons
			fullFrame: (LayoutFrame identity topFraction: 1;
							 topOffset: 1 - (buttonSize + 1));
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> buildSetUpCodeFor: aClass [ 
	
	^ String streamContents: [:str | str << 'setUp';cr.
		str tab << '"Setting up code for '<<aClass name<<'"';cr;cr.
		aClass instVarNames sorted do: [:name | str cr tab << name << ' := nil.' ]].
	
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> buildTearDownCodeFor: aClass [ 
	
	^ String streamContents: [:str | str << 'tearDown';cr.
		str tab << '"Tearing down code for '<<aClass name<<'"';cr;cr.
		aClass instVarNames sorted do: [:name | str cr tab << name << ' := nil.' ]].
	
]

{ #category : #'test creation' }
AbstractNautilusUI >> buildTestClassDefinitionFrom: aClass [

	^ 'TestCase subclass: ', (self buildTestClassNameFrom: aClass) printString, '
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	package: ''',(self buildTestPackageNameFrom:aClass),''''
]

{ #category : #'test creation' }
AbstractNautilusUI >> buildTestClassNameFrom: aClass [
	
	^ (aClass name asString,'Test') asSymbol
]

{ #category : #'test creation' }
AbstractNautilusUI >> buildTestPackageNameFrom:aClass [

	^ aClass package name asString, '-Tests' 
]

{ #category : #'build ui lists' }
AbstractNautilusUI >> buildThirdColumn: aWindow [

	^ self buildCategoryWidget
]

{ #category : #'build ui' }
AbstractNautilusUI >> buildTopBarOn: aWindow height: topHeight [
	aWindow
		addMorph: self buildButtonsBar
		fullFrame:
			(LayoutFrame identity
				topOffset: topHeight;
				bottomFraction: 0;
				bottomOffset: self buttonsBarHeight;
				leftOffset: -1;
				rightOffset: 1)
]

{ #category : #'build ui plugins' }
AbstractNautilusUI >> buildTopPlugins [

	| top container |
	top := self model plugins select: [:each | each position = #top ].
	top ifEmpty: [ ^ nil ].
	top size = 1 ifTrue: [ ^ top first display ].
	container := Morph new.
	container 
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #topToBottom;
		cellPositioning: #topLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	top reverse do: [:each |
		each display ifNotNil: [:morph | container addMorph: morph ]].
	^ container
]

{ #category : #'button-variables' }
AbstractNautilusUI >> buildVariableMenuFor: aMenu variablesFrom: aBlockOrSymbol menuItemBy: aMenuItemBlock [

	self selectedClass withAllSuperclasses reverseDo: [ :class |
		| vars |
		
		vars := aBlockOrSymbol value: class. 
		
		self variablesMenuEntry: class 
			items: vars 
			in: aMenu 
			using: aMenuItemBlock.
	].

	
]

{ #category : #displaying }
AbstractNautilusUI >> buildWindow [
	window := NautilusWindow new model: self.
	self addAll: window.
	self setWindowTitle.
	self registerToAnnouncements.
	self selectedMethod ifNotNil: [ self resetIconStyle  ].
	window announcer when: WindowClosed send: #unregisterToAnnouncements to: self
]

{ #category : #'build ui' }
AbstractNautilusUI >> buttonsBarHeight [
	^ StandardFonts defaultFont pixelSize + 14
]

{ #category : #breakpoints }
AbstractNautilusUI >> canInstallMethodBreakpoint [

	| result |
	
	self selectedMethod ifNil: [ ^ false ].

	result := self sourceTextModel hasUnacceptedEdits not.
	
	result ifFalse: [ self inform: 'You need to save the method to install a Breakpoint' ].
	
	^ result
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> categorizeAllUncategorizedMethods [
	"Categorize methods by looking in parent classes for a method category.
	In addition, it automatically removed empty categories"
	self selectedClass ifNotNil: [ :aClass | | methods |
		methods := aClass uncategorizedSelectors collect: [ :selector| aClass >> selector ].
		self model classifier classifyAll: methods ].
	self removeEmptyCategories
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> categorizeMethod [
	^ self subclassResponsibility
]

{ #category : #'menu builder' }
AbstractNautilusUI >> categoryMenuBuilder [

	^ PragmaMenuBuilder withAllPragmaKeywords: {self commonPragma. self categoryPragma} model: self
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> categoryPragma [
	^'nautilusGlobalProtocolMenu'
]

{ #category : #'widget category' }
AbstractNautilusUI >> categoryWidget [
	self subclassResponsibility 
]

{ #category : #updating }
AbstractNautilusUI >> changed: aSymbol [

	super changed: aSymbol.
	self announce: (NautilusChanged symbol: aSymbol)
]

{ #category : #'system announcements' }
AbstractNautilusUI >> classCommented: anAnnouncement [
    " announcement handled when a class has been commented " 
    | aClass |

    window ifNil: [ ^ self ].
    window isDisplayed ifFalse: [ ^ self ].

    aClass := anAnnouncement classCommented.    
    (self classWidget getClassesList includes: aClass)
        ifTrue: [ self updateClassView ]
]

{ #category : #'system announcements' }
AbstractNautilusUI >> classDefinitionModified: anAnnouncement [
	| class |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	class := anAnnouncement oldClassDefinition.
	self removeClassFromMethodsIconsCache: class.
	(self classWidget getClassesList includes: class)
		ifTrue: [ model selectedCategory
				ifNil: [ model selectedMethod
						ifNil: [ model selectedClass = anAnnouncement oldClassDefinition
								ifTrue: [ model selectedClass: anAnnouncement newClassDefinition ].	"Minimal change to fix Case13006. Further investigation required in Pharo 4 on Case13020"
							self updateClassView.
							self sourceTextModel hasUnacceptedEdits
								ifFalse: [ self updateSourceCodeAreaWith: self sourceCode ].
							self removeFromPackagesIconsCache: class package ] ] ]
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> classFixPragma [
	^'nautilusGlobalClassFixMenu'
]

{ #category : #icon }
AbstractNautilusUI >> classIconFor: aClass [
	(aClass isTestCase and: [ aClass isAbstract not and: [ aClass hasTestSelectors ] ])
		ifTrue: [ | icon button |
			icon :=  self iconNamed: #testNotRunIcon.
			aClass hasPassedTest ifTrue: [ icon := self iconNamed: #testGreenIcon ].
			aClass hasFailedTest ifTrue: [ icon := self iconNamed: #testYellowIcon ].
			aClass hasErrorTest ifTrue: [ icon := self iconNamed: #testRedIcon ].
					
			button := IconicButton new 
						target: self;
						actionSelector: #runTestsOfClass:notifying:;
						arguments: { aClass. true };
						labelGraphic: icon ;
						color: Color transparent;
						extent: 12 @ 12;
						helpText: 'Run the tests';
						borderWidth: 0.
			^ button ]
		ifFalse: [
			(aClass hasComment not )
				ifTrue: [| button |
					button := IconicButton new 
								target: self;
								actionSelector: #openCommentEditor:;
								arguments: { aClass };
								labelGraphic: (self iconNamed: #uncommentedClassIcon) ;
								color: Color transparent;
								extent: 12 @ 12;
								helpText:'Edit the comment';
								borderWidth: 0.
					^ button ]].

	^ aClass systemIcon
]

{ #category : #'menu builder' }
AbstractNautilusUI >> classMenuBuilder [

	^ PragmaMenuBuilder withAllPragmaKeywords: {self classFixPragma. self classPragma} model: self
]

{ #category : #'system announcements' }
AbstractNautilusUI >> classParentRenamed: anAnnouncement [
	| class |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	class := anAnnouncement classAffected.
	(self selectedClass = class)
		ifTrue:
			[ 
			self updateClassView.
			self sourceTextModel hasUnacceptedEdits
				ifFalse: [ self updateSourceCodeAreaWith: self sourceCode ] ]
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> classPragma [
	^'nautilusGlobalClassMenu'
]

{ #category : #'system announcements' }
AbstractNautilusUI >> classRecategorized: anAnnouncement [
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	self selectedPackage ifNotNil:[:selection |
	({ anAnnouncement oldCategory. anAnnouncement newCategory }
		anySatisfy: [ :each | selection includesCategory: each ])
			ifTrue: [ self updatePackageGroupAndClassList ]]
]

{ #category : #'system announcements' }
AbstractNautilusUI >> classRemoved: anAnnouncement [
	| class oldPackage |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	class := anAnnouncement classRemoved.
	(self classWidget getClassesList includes: class)
		ifTrue:
			[ 
			self selectedClass
				ifNotNil:
					[ :selectedClass | 
					selectedClass theNonMetaClass = class
						ifTrue: [ self selectedClass: nil ] ].
			window isDisplayed
				ifFalse: [ ^ self ].
			oldPackage := RPackageOrganizer default
				packageMatchingExtensionName: anAnnouncement categoryName.
			self removeFromPackagesIconsCache: oldPackage.
			self updatePackageGroupAndClassList ]
]

{ #category : #'system announcements' }
AbstractNautilusUI >> classRenamed: anAnnouncement [
	| class |
	class := anAnnouncement classRenamed.
	self setWindowTitle.
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	(self classWidget getClassesList includes: class)
		ifTrue:
			[ 
			self updateClassView.
			self sourceTextModel hasUnacceptedEdits
				ifFalse: [ self updateSourceCodeAreaWith: self sourceCode ] ]
]

{ #category : #'system announcements' }
AbstractNautilusUI >> classReorganized: anAnnouncement [
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	(self selectedClass = anAnnouncement classReorganized or: [ anAnnouncement classReorganized users includes: self selectedClass  ])
		ifTrue: [ self updatePackageGroupAndClassList ]
]

{ #category : #'widget class' }
AbstractNautilusUI >> classWidget [
	self subclassResponsibility 
]

{ #category : #'menu builder' }
AbstractNautilusUI >> classesMenu: aMenu shifted: aBoolean [ 

	^ aMenu addAllFrom: self classMenuBuilder menu.
]

{ #category : #'source code area' }
AbstractNautilusUI >> clearUserEditFlag [
	
	self changed: #clearUserEdits
]

{ #category : #displaying }
AbstractNautilusUI >> close [
	window close
]

{ #category : #'source code area' }
AbstractNautilusUI >> codePaneMenu: aMenu shifted: shifted [ 
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"
	| donorMenu |
	donorMenu := shifted
		ifTrue: [SmalltalkEditor shiftedYellowButtonMenu]
		ifFalse: [SmalltalkEditor yellowButtonMenu].
	^ aMenu addAllFrom: donorMenu
]

{ #category : #'browser compatibility' }
AbstractNautilusUI >> codeTextMorph [

	^ sourceTextWidget 
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> commentButtonAction: aBoolean [
	self toggleShowComment
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> commentButtonIcon [
	^ self iconNamed: #commentsIcon
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> commentButtonLabel [
	^ ' Com.'
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> commentButtonState [

	^ self showComment
]

{ #category : #'comment pane' }
AbstractNautilusUI >> commentTextModel [
	^ commentTextModel
		ifNil: [ 
			commentTextModel := RubScrolledTextModel new
				interactionModel: self;
				yourself ]
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> commonPragma [
	^'nautilusGlobalCommonMenu'
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> compareTwoMethods [
	^ self subclassResponsibility
]

{ #category : #'source code area' }
AbstractNautilusUI >> compileAClassFrom: aString notifying: aController [
	self sourceTextModel clearUserEdits.
	(self
		compileANewClassFrom: aString
		notifying: aController
		startingFrom: self selectedClass)
		ifNil: [ ^ self sourceTextModel hasUnacceptedEdits: true ]
		ifNotNil:
			[ :class | 
			self selectedPackage: (self parentOfClass: class).
			self selectedClass: class.
			self updatePackageGroupAndClassList ]
]

{ #category : #'source code area' }
AbstractNautilusUI >> compileAMethodFromCategory: aCategory withSource: aString notifying: aController [
	^ self subclassResponsibility
]

{ #category : #'source code area' }
AbstractNautilusUI >> compileSource: aText notifying: aController [
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractNautilusUI >> contentSelection [
	^ contentSelection
]

{ #category : #private }
AbstractNautilusUI >> copyCategory: anObject toTheClass: aClass [
	
	| originClass methods |
	originClass := self selectedClass.
	methods := originClass methodsInProtocol: anObject.
	methods do: [:meth | self copyMethod: meth toTheClass: aClass ].
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> copyClasses [
	
	| association |
	self okToChange ifFalse: [^ self].
	association := self copyClasses: (self selectedClasses collect: [:e | e theNonMetaClass ]).
	association key
		ifTrue: [
			self selectedClass: association value.
			self updateClassView]
]

{ #category : #private }
AbstractNautilusUI >> copyMethod: aMethod toTheClass: aClass [ 
	| newMethod |
	
	aMethod realClass = aClass ifTrue: [ ^ self ].
	aClass methodDict
						at: aMethod selector
						ifPresent: [:sel | 
							" here I have to fork to release the drag & drop "
							[ (self openDialogWouldYouInstall: sel into: aClass)
								ifTrue: [ 
									newMethod := aClass compile: aMethod sourceCode classified: aMethod category ]] fork.
							  ]
						ifAbsent: [ 
							newMethod := aClass compile: aMethod sourceCode classified: aMethod category]
]

{ #category : #'test creation' }
AbstractNautilusUI >> createTestForClass: aClass [
	| definition testClass className |
	aClass ifNil: [ ^ nil ].
	aClass isTestCase
		ifTrue: [ ^ nil ].
	aClass isMeta
		ifTrue: [ ^ nil ].
	className := self buildTestClassNameFrom: aClass.
	testClass := self class environment
		at: className
		ifPresent: [ :class | class ]
		ifAbsent:
			[ 
			definition := self buildTestClassDefinitionFrom: aClass.
			testClass := self
				compileANewClassFrom: definition
				notifying: nil
				startingFrom: self selectedClass.
			self class environment at: className.
			testClass
				comment:
					(self generateCommentForTestClass: testClass from: aClass) ].
	self selectedPackage: (self parentOfClass: testClass).
	self selectedClass: testClass.
	self updatePackageGroupAndClassList.
	^ testClass
]

{ #category : #'test creation' }
AbstractNautilusUI >> createTestForSelectedClass [
 
	self createTestForClass: self selectedClass theNonMetaClass 
]

{ #category : #'source code area' }
AbstractNautilusUI >> defaultClassDescriptor [
	| string |
	string := Slot showSlotClassDefinition 
		ifTrue: [  
'Object subclass: #NameOfSubclass
	slots: {}
	classVariables: {}
	category: ''']
		ifFalse: [
'Object subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' ].
	^ self selectedPackage
		ifNil: [string, '''' ]
		ifNotNil: [:package | string, package name, '''' ]
]

{ #category : #'menu-packages' }
AbstractNautilusUI >> demoteSelectedPackageAsPackageWithTag [
	self subclassResponsibility
]

{ #category : #'source code area' }
AbstractNautilusUI >> doItContext [

	^ nil
]

{ #category : #'source code area' }
AbstractNautilusUI >> doItReceiver [

	^ self selectedClass 
		ifNotNil: [:class | class theNonMetaClass ]
]

{ #category : #'events handling' }
AbstractNautilusUI >> doubleClickOnPackage [
	self selectedPackage ifNil: [ ^ self ] ifNotNil: [ :package | self model class openOnPackage: package ]
]

{ #category : #'drag and drop' }
AbstractNautilusUI >> dragPassengersFor: item inMorph: dragSource [
	| transferType object |
	object := item originalObject.
	(dragSource isKindOf: AbstractWidget listClass)
		ifFalse: [ ^ nil ].
	transferType := self dragTransferTypeForMorph: dragSource.
	transferType == #getCategoryItem:
		ifTrue: [ ^ self selectedCategories ifEmpty: [ {object} ] ].
	transferType == #getClassItem:
		ifTrue: [ ^ self selectedClasses ifEmpty: [ {object} ] ].
	transferType == #getMethodItem:
		ifFalse: [ ^ nil ].
	^ self selectedMethods
		ifEmpty:
			[ 
			(self getMethods includes: object)
				ifTrue: [ {object} ]
				ifFalse: [ nil ] ]
]

{ #category : #'drag and drop' }
AbstractNautilusUI >> dragTransferTypeForMorph: dragSource [ 

	^(dragSource isKindOf: AbstractWidget listClass)
		ifTrue: [ dragSource getListElementSelector ]
]

{ #category : #'drag and drop' }
AbstractNautilusUI >> dropInAClass: aCollection into: aClass [

	aCollection do: [:anObject | 
		(anObject isBehavior)
			ifFalse: [
				(anObject isString or: [ anObject isSymbol ])
					ifTrue: ["category"
						ActiveHand shiftPressed
							ifTrue: [ self copyCategory: anObject toTheClass: aClass ]
							ifFalse: [ self moveCategory: anObject toTheClass: aClass ]]
					ifFalse: [
						"a method"
						ActiveHand shiftPressed
							ifTrue: [ self copyMethod: anObject toTheClass: aClass ]
							ifFalse: [ self moveMethod: anObject toTheClass: aClass autoRemove: false ]]]]
]

{ #category : #'drag and drop' }
AbstractNautilusUI >> dropInAPackage: aCollection into: receiver [
	aCollection
		do: [ :aClassOrAMethod | aClassOrAMethod dropIn: receiver forNautilusUI: self ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> enableSingleClassSelection [

	^ self selectedClasses size <= 1
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> enableSingleMenuItems [

	^ self packageWidget selectedPackages size <= 1
]

{ #category : #private }
AbstractNautilusUI >> extensionColor [
	^ Smalltalk ui theme classExtensionColor
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> fileOutClass [
	"Print a description of the selected class onto a file whose name is the 
	category name followed by .st."

	self fileOutClasses: self selectedClasses
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> fileOutMethods [
	^ self subclassResponsibility
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> fileOutPackage [

	self fileOutPackages: self selectedPackages
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> findClass [
	"Search for a class from a pattern or from the recent list"
	self findClass: (SearchFacade classSearchInEnvironment: self browsedEnvironment).
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> findClass: aSearchInstance [
	"select the class returned by the search block"

	| aClass |
	self okToChange
		ifFalse: [ ^ self flashPackage ].
	aClass := aSearchInstance chooseFromOwner: self window.
	aClass ifNil: [ ^ self flashPackage ].
	self selectedPackage: (self parentOfClass: aClass).
	self selectedClass: aClass.
	self updatePackageGroupAndClassList
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> findClassInHierarchy [
	"Search for a subclass of the selected class from a pattern or from the recent list"
	
	self selectedClass ifNotNil: [ :class| 
		self findClass: (SearchFacade hierarchySearchFor: class theNonMetaClass)].
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> findPackage [

	"Search for a package from a pattern or from the recent list"
	| foundPackage |
	self okToChange ifFalse: [^ self ].	
	foundPackage := SearchFacade rPackageSearch chooseFromOwner: self window.
	foundPackage ifNil: [ ^self ].
	self selectedPackage: foundPackage.
	self updatePackageViewAndMove.

]

{ #category : #'menus behavior' }
AbstractNautilusUI >> findSubclass [
	"Search for a subclass of the selected class from a pattern or from the recent list"
	
	self selectedClass ifNotNil: [ :class| 
		self findClass: (SearchFacade subclassSearchFor: class theNonMetaClass)].
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> findSuperclass [
	"Search for a superclass of the selected class from a pattern or from the recent list"
	self selectedClass ifNotNil: [ :class| 
		self findClass: (SearchFacade superclassSearchFor: class theNonMetaClass)].
]

{ #category : #private }
AbstractNautilusUI >> flashPackage [

	^ nil
]

{ #category : #'source code area' }
AbstractNautilusUI >> flashSourceCodeArea [

	self sourceTextModel reconfigureViewWith: [ :scrolledText | scrolledText textArea flash ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> forceGenerateInitialize [

	self selectedClass ifNotNil: [:class || code |
		(class isMeta not and: [class isTestCase])
			ifTrue: [
				code := self buildSetUpCodeFor: class.
				class compile: code classified: 'initialization'.
				code := self buildTearDownCodeFor: class.
				class compile: code classified: 'initialization'.
				self selectedMethod: (class>>#setUp) ]
			ifFalse: [
				code := self buildInitializeCodeFor: class.
				class compile: code classified: 'initialization' .
				self selectedMethod: (class>>#initialize) ].
		self updateCategoryAndMethodList.
		self giveFocusToSourceCodeArea ]
]

{ #category : #'widget method' }
AbstractNautilusUI >> forceSelectedMethod: aMethod [

	^ self subclassResponsibility
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> fullBrowse [
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractNautilusUI >> fullDisplayedComment [
	^ fullDisplayedComment
]

{ #category : #accessing }
AbstractNautilusUI >> fullDisplayedComment: anObject [
	fullDisplayedComment := anObject
]

{ #category : #'test creation' }
AbstractNautilusUI >> generateCommentForTestClass: testClass from: aClass [

	^ String streamContents: [:stream || name |
			name := testClass name. 
			name first isVowel
				ifTrue: [ stream << 'An ']
				ifFalse:[ stream <<'A '].
			stream << name << ' is a test class for testing the behavior of '<< aClass name ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> generateInitialize [
	
	self selectedClass ifNotNil: [:class || code protocol |
		protocol := 'initialization'.
		(class isMeta not and: [class isTestCase])
			ifTrue: [
				(class includesSelector:  #setUp) 
					ifFalse: [
						code := self buildSetUpCodeFor: class.
						class compile: code classified: protocol ].
				(class includesSelector: #tearDown) 
					ifFalse: [
						code := self buildTearDownCodeFor: class.
						class compile: code classified: protocol ].
				self selectedMethod: (class>>#setUp) 
				]
			ifFalse: [
				(class includesSelector: #initialize) 
					ifFalse: [
						code := self buildInitializeCodeFor: class.
						class compile: code classified: protocol ].
				self selectedMethod: (class>>#initialize) ].
		self updateCategoryAndMethodList.
		self giveFocusToSourceCodeArea ]
]

{ #category : #'source code area' }
AbstractNautilusUI >> getComments [
	^ self selectedClass
		ifNil: [ self selectedPackage
				ifNil: [ '' ]
				ifNotNil: [ :package | 
					package isGroup
						ifTrue: [ '' ]
						ifFalse: [ package packageComment ] ] ]
		ifNotNil: [ :class | class comment ]
]

{ #category : #private }
AbstractNautilusUI >> getPackagesWithoutExtensionsList [
	
	^ self model packagesWithoutExtensions sort: [:a :b | a name <= b name ]
]

{ #category : #private }
AbstractNautilusUI >> giveFocusTo: aMorph [

	self hasFocus ifTrue: [aMorph takeKeyboardFocus]
]

{ #category : #focus }
AbstractNautilusUI >> giveFocusToPackage [

	self giveFocusTo: self packageWidget
]

{ #category : #focus }
AbstractNautilusUI >> giveFocusToSourceCodeArea [
	self sourceTextModel 
		reconfigureViewWith: [ :scrolledText | 
			scrolledText textArea takeKeyboardFocus ]
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> groupFixPragma [
	^'nautilusGlobalGroupFixMenu'
]

{ #category : #icon }
AbstractNautilusUI >> groupIconFor: aGroup [
	| icon |
	
	icon := IconicButton new 
				target: self;
				actionSelector: #restrictedBrowseGroups:;
				arguments: {{aGroup}};
				labelGraphic: (self iconNamed: #groupIcon);
				color: Color transparent;
				extent: 15 @ 16;
				helpText: 'Browse restricted environment';
				borderWidth: 0. 
	^ icon
]

{ #category : #'menu builder' }
AbstractNautilusUI >> groupMenuBuilder [

	^ PragmaMenuBuilder withAllPragmaKeywords: {self groupFixPragma. self groupPragma} model: self
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> groupPragma [
	^'nautilusGlobalGroupMenu'
]

{ #category : #private }
AbstractNautilusUI >> groupsLabel [

	^ 'Groups'
]

{ #category : #accessing }
AbstractNautilusUI >> groupsManager [
	^ self model groupsManager
]

{ #category : #'menu builder' }
AbstractNautilusUI >> groupsMenu: aMenu shifted: aBoolean [ 
	" Morphic's menus are filled up when pragma's one are returned from a method, so I have to substitute the morphic menu by the pragma one "
	
	^ aMenu addAllFrom: (self groupMenuBuilder menu)
]

{ #category : #displaying }
AbstractNautilusUI >> hasFocus [

	^ window hasKeyboardFocus or: 
		[ self packageWidget hasFocus or:
		[ self classWidget hasFocus or: 
		[ self categoryWidget hasFocus or:
		[ self methodWidget hasFocus "or:
		[ sourceTextArea hasKeyboardFocus ]"]]]]
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> hierarchyButtonAction: aBoolean [

	self hierarchyButtonEnabled
		ifTrue: [ 
			self toggleShowHierarchy.
			self changed: #hierarchyButtonEnabled. ]
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> hierarchyButtonEnabled [
	^ self selectedClass notNil or: [ self showHierarchy ]
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> hierarchyButtonIcon [
	^ self iconNamed: #hierarchyIcon
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> hierarchyButtonLabel [
	^ ' Hier.'
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> hierarchyButtonState [

	^ self showHierarchy
]

{ #category : #'source code area' }
AbstractNautilusUI >> highlight: autoSelectString [
	| first |

	first := self sourceTextModel getString findString: autoSelectString.
	contentSelection := first to: first+autoSelectString size-1.
	self updateSourceCodeAreaWith: self sourceCode
]

{ #category : #initialization }
AbstractNautilusUI >> initialize [
	super initialize.
	shouldUpdateTitle := true.
	contentSelection := nil.
	acceptor := ClassDefinitionAcceptor model: self.
	fullDisplayedComment := false
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> inspectMethod [
	 self selectedMethod ifNil: [ ^ self ].
	 self selectedMethod inspect
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> inspectMethods [
	 self selectedClass ifNil: [ ^ self ].
	 self selectedClass methods inspect
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> instanceButtonAction: aBoolean [
	self toggleShowInstance
]

{ #category : #accessing }
AbstractNautilusUI >> instanceButtonHelpTextFor: buttonStateBoolean [

	^ buttonStateBoolean 
		ifFalse: [ 'Show the class side' ]
		ifTrue: [ 'Show the instance side' ]

]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> instanceButtonIcon [
	^ self iconNamed: #classIcon
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> instanceButtonLabel [
	| label |
	label := 'Class' asMorph emphasis: 2.
	^ self selectedClass
		ifNil: [ label ]
		ifNotNil: [ :class | 
			class theMetaClass hasMethods
				ifTrue: [ label emphasis: 1 ]
				ifFalse: [ label ] ]
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> instanceButtonState [

	^ "false" self showInstance not
]

{ #category : #accessing }
AbstractNautilusUI >> instanceButtonWidget [
	^ instanceButtonWidget 
]

{ #category : #private }
AbstractNautilusUI >> isAClassSelected [

	^ self selectedClass notNil
]

{ #category : #'events handling' }
AbstractNautilusUI >> keyPressedOnElement: anEvent [

	^ self subclassResponsibility
]

{ #category : #'events handling' }
AbstractNautilusUI >> listMouseDown: anEvent [

	self showHierarchy
		ifTrue: [ 
			self showHierarchy: false.
			self packageWidget mouseDown: anEvent ].

]

{ #category : #private }
AbstractNautilusUI >> loadList [
	
	^  self getPackagesWithoutExtensionsList
]

{ #category : #'monticello announcements' }
AbstractNautilusUI >> mcPackageModified: anAnnouncement [
	" handled when a package become dirty "
	
	| rpackages |
	window ifNil: [^ self ].
	window isDisplayed ifFalse: [ ^ self ].

	rpackages := anAnnouncement package packageSet packages.
	rpackages isEmptyOrNil ifTrue: [ ^ self ].
	(self model packages includesAnyOf: rpackages )
		ifTrue: [ 
			rpackages do: [:rpackage | PackagesIconsCache removeKey: rpackage ifAbsent: []].
			self updatePackageView ]
]

{ #category : #'monticello announcements' }
AbstractNautilusUI >> mcWorkingCopyCreated: anAnnouncement [
	| package rpackage |
	window ifNil: [^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	package := anAnnouncement package.
	package ifNil: [ ^ self ].
	rpackage := RPackageOrganizer default packageNamed: package name.
	(self model packages includes: rpackage)
		ifTrue: [ 
			PackagesIconsCache removeKey: rpackage  ifAbsent: [].
			self updatePackageView ]
]

{ #category : #'monticello announcements' }
AbstractNautilusUI >> mcWorkingCopyDeleted: anAnnouncement [
	| package rpackages |
	
	window ifNil: [^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	package := anAnnouncement package.
	package ifNil: [ ^ self ].
	rpackages := package packageSet packages.
	rpackages isEmptyOrNil ifTrue: [ ^ self ].
	rpackages do: [:rpackage | 
		PackagesIconsCache removeKey: rpackage ifAbsent: []].
	self updatePackageView
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> mergeGroups [

	| group |
	group := self mergeGroups: self packageWidget selectedGroups.
	group
		ifNotNil: [ 
			self groupsManager addAGroup: group.
			self selectedGroup: group.
			self updatePackageGroupAndClassList]

]

{ #category : #'system announcements' }
AbstractNautilusUI >> methodAdded: anAnnouncement [

	^ self subclassResponsibility
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> methodHierarchy [
	"Create and schedule a method browser on the hierarchy of implementors."

	self methodHierarchyFrom: self selectedMethod
]

{ #category : #'menu builder' }
AbstractNautilusUI >> methodMenuBuilder [

	^ PragmaMenuBuilder withAllPragmaKeywords: {self commonPragma. self methodPragma} model: self
]

{ #category : #'system announcements' }
AbstractNautilusUI >> methodModified: anAnnouncement [ 
	
	^ self subclassResponsibility 
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> methodPragma [
	^'nautilusGlobalMethodMenu'
]

{ #category : #'system announcements' }
AbstractNautilusUI >> methodRecategorized: anAnnouncement [
	
	^ self subclassResponsibility
]

{ #category : #'system announcements' }
AbstractNautilusUI >> methodRemoved: anAnnouncement [

	^ self subclassResponsibility
]

{ #category : #'widget method' }
AbstractNautilusUI >> methodWidget [
	self subclassResponsibility 
]

{ #category : #'widget method' }
AbstractNautilusUI >> methodsForCategories: aCollection [

	^ self subclassResponsibility
]

{ #category : #'widget method' }
AbstractNautilusUI >> methodsForCategory: aCategory [
	^ self subclassResponsibility
]

{ #category : #'widget method' }
AbstractNautilusUI >> methodsIconsCache [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractNautilusUI >> model [

	^ model
]

{ #category : #accessing }
AbstractNautilusUI >> model: aNautilusModel [

	model := aNautilusModel.

	aNautilusModel selectedPackage ifNotNil: [:e | self packageWidget selectPackage: e].
	aNautilusModel selectedGroup ifNotNil: [:p | self packageWidget selectGroup:p].
	aNautilusModel selectedClass ifNotNil: [:p | self classWidget selectClass: p].
]

{ #category : #private }
AbstractNautilusUI >> moveCategory: anObject toTheClass: aClass [
	
	| originClass methods |
	originClass := self selectedClass.
	methods := originClass methodsInProtocol: anObject.
	methods do: [:meth | self moveMethod: meth toTheClass: aClass autoRemove: true ].
	
	(originClass methodsInProtocol: anObject)
		ifEmpty: [ 
			originClass removeProtocol: anObject.
			self selectedCategory: nil ].
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> moveInPackage [

	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"
	| foundPackage |
	self okToChange ifFalse: [^ self].
	foundPackage := SearchFacade rPackageSearch chooseFromOwner: self window.
	foundPackage ifNil: [ ^self ].
	self selectedClasses 
		do: [:aClass | aClass category: foundPackage name ].
		self flag: #esteban. "unclear that we should still use category:"
	 self selectedPackage: foundPackage.
	self updatePackageGroupAndClassList
]

{ #category : #private }
AbstractNautilusUI >> moveMethod: aMethod toTheClass: aClass autoRemove: autoRemove [
	
	aMethod realClass = aClass ifTrue: [ ^ self ].
	aClass methodDict
						at: aMethod selector
						ifPresent: [:sel | 
							" here I have to fork to release the drag & drop "
							[ (self openDialogWouldYouInstall: sel into: aClass)
								ifTrue: [ | originClass oldCategory |
									oldCategory := aMethod category.
									originClass := aMethod realClass.
									aClass compile: aMethod sourceCode classified: oldCategory.
									self selectedMethod: nil.
									
									originClass removeSelector: aMethod selector.
									autoRemove
										ifTrue: [ (originClass selectorsInProtocol: oldCategory )
													ifEmpty: [ originClass removeProtocol: oldCategory ]]]] fork]
						ifAbsent: [ 
							 | originClass oldCategory |
							oldCategory := aMethod category.
							originClass := aMethod realClass.
							aClass compile: aMethod sourceCode classified: oldCategory.
							self selectedMethod: nil.
							originClass removeSelector: aMethod selector.
							autoRemove
								ifTrue: [
									( originClass selectorsInProtocol: oldCategory )
										ifEmpty: [ originClass removeProtocol: oldCategory ]]]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> moveMethodToPackage [
	^ self subclassResponsibility
]

{ #category : #'build ui' }
AbstractNautilusUI >> navigationAreaProportion [
	^ 0.47
]

{ #category : #default }
AbstractNautilusUI >> noMethodsString [

	^ Protocol nullCategory
]

{ #category : #private }
AbstractNautilusUI >> notifyTestResultFrom: packages classes: classes labeled: label [
	| color |
	color := Color gray.
	(classes anySatisfy: [ :e | e hasPassedTest ])
		ifTrue: [ color := self theme successBackgroundColor ].
	(classes anySatisfy: [ :e | e hasFailedTest ])
		ifTrue: [ color := self theme warningBackgroundColor ].
	(classes anySatisfy: [ :e | e hasErrorTest ])
		ifTrue: [ color := self theme dangerBackgroundColor ].
	^ self
		notifyTitle: 'Test Finished'
		contents: label , ((packages collect: #name) joinUsing: ', ')
		color: color
]

{ #category : #'widget method' }
AbstractNautilusUI >> notifyTitle: title contents: contents color: aColor [
	^ self subclassResponsibility
]

{ #category : #updating }
AbstractNautilusUI >> okToChange [

	| answer |
	
	self sourceTextModel ifNil: [ ^ true ].
	
	(self sourceTextModel canDiscardEdits and: [ 
		RBProgramNode formatterClass formatAsYouReadPolicy or: [ 
			(self sourceTextModel getString trimBoth = 
				sourceTextWidget text asString trimBoth)
		]
	]) ifTrue: [ ^ true ].	
	
	answer := self promptForCancel.
	answer ifNotNil: [ 
		answer
			ifTrue: [ self acceptSourceCodeFromView ]
			ifFalse: [ self sourceTextModel cancelEdits ] 
	].
			
	^ self sourceTextModel canDiscardEdits
	
]

{ #category : #'source code area' }
AbstractNautilusUI >> okToChangeComment [

	| answer |
	commentTextModel canDiscardEdits ifTrue: [^ true].
	answer :=  self promptForCancel.
	answer ifNil: [ ^ commentTextModel canDiscardEdits ];
			ifTrue: [ self acceptCommentFromView  ]
			ifFalse: [  commentTextModel cancelEdits  ].	
	^ commentTextModel canDiscardEdits

]

{ #category : #'source code area' }
AbstractNautilusUI >> okToChangeSourceAndComment [

	^ self okToChange and: [ self okToChangeComment ]
]

{ #category : #displaying }
AbstractNautilusUI >> open [
	self buildWindow.
	window openInWorld
	
]

{ #category : #breakpoints }
AbstractNautilusUI >> openBreakpointMenuFor: aNode [

	| menu |
	
	menu := MenuMorph new defaultTarget: self.
	menu add: 'Break' translated selector: #breakOn: argument: aNode.
	menu lastItem icon: (self iconNamed: #stop).
	menu add: 'Break once' translated selector: #breakOnceOn: argument: aNode.
	menu lastItem icon: (self iconNamed: #stop).
	menu add: 'Break on condition' translated selector: #breakOnConditionOn: argument: aNode.
	menu lastItem icon: (self iconNamed: #stop).
	
	menu popUpInWorld
]

{ #category : #'plugins announcements' }
AbstractNautilusUI >> openClass: aClass [
	aClass ifNil: [ ^ self ].
	self open browseClass: aClass
]

{ #category : #'icons behavior' }
AbstractNautilusUI >> openCommentEditor: aClass [
	| newComment |

	newComment := Smalltalk ui theme
		textEditorIn: window
		text: 'Enter a comment for the class ', aClass theNonMetaClass name,':'
		title: 'Comment Editor'
		entryText: aClass theNonMetaClass comment
		entryHeight: 300.
	
	newComment
		ifNotNil: [ aClass theNonMetaClass comment: newComment asString ]
]

{ #category : #private }
AbstractNautilusUI >> openDialogWouldYouInstall: aMethod into: aClass [

	"open a dialog to ask user if he wants to override the existing method or not"
	^ UIManager default confirm: 'Are you sure you want to override the method ', aMethod selector, ' in ', aClass name label: 'Override' 
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> openGroupManager [
	(PackageCreatorTreeModel new groups: self groupsManager) open
]

{ #category : #shortcuts }
AbstractNautilusUI >> openShortcutDescription [

	KMDescription new
		categories: #(NautilusGlobalShortcuts "NautilusClassShortcuts NautilusSourceCodeShortcuts NautilusPackageShortcuts NautilusProtocolShortcuts NautilusMethodShortcuts") sort;
		openWithSpec
]

{ #category : #'plugins announcements' }
AbstractNautilusUI >> openSubclass [
	"Search for a superclass of the selected class from a pattern or from the recent list"

	self selectedClass ifNotNil: [ :class| | search |
		search := SearchFacade subclassSearchFor: class theNonMetaClass.
		self openClass: (search chooseFromOwner: self window) ].
]

{ #category : #'plugins announcements' }
AbstractNautilusUI >> openSuperclass [
	"Search for a superclass of the selected class from a pattern or from the recent list"

	self selectedClass ifNotNil: [ :class| 
		self openClass: class superclass ].
]

{ #category : #'system announcements' }
AbstractNautilusUI >> packageChanged: anAnnouncement [
	window isDisplayed
		ifFalse: [ ^ self ].
	self updatePackageView
]

{ #category : #'comment pane' }
AbstractNautilusUI >> packageCommentEmptyOrUnchanged [
	^ commentTextModel getText isEmpty
		or: [ commentTextModel getText asString = PackageManifest description ]
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> packageFixPragma [
	^ 'nautilusGlobalPackageFixMenu'
]

{ #category : #icon }
AbstractNautilusUI >> packageIconFor: aPackage [
	| mcpackage |
	
	PackagesIconsCache
		at: aPackage
		ifPresent: [:packageIcon |
			packageIcon class == IconicButtonStateHolder
				ifTrue: [
					"convert the IconicButtonStateHolder previously cached by
					this method before returning"
					^ packageIcon asIcon ].
			^ packageIcon ].

	aPackage classes isEmpty 
		ifTrue: [^ PackagesIconsCache at: aPackage put: (self iconNamed: #emptyPackageIcon) ].
	
	mcpackage := aPackage mcPackage.
	(mcpackage notNil and: [ mcpackage isDirty ])
		ifTrue: [ | icon |
			icon := IconicButton new 
						target: NautilusUI;
						actionSelector: #saveDirtyPackages:;
						arguments: {{ aPackage }};
						labelGraphic: (aPackage definedClasses
											ifEmpty: [ (self iconNamed: #dirtyMonticelloPackageIcon) ]
											ifNotEmpty: [ (self iconNamed: #dirtyPackageIcon) ]);
						color: Color transparent;
						extent: 15 @ 16;
						helpText: 'Save the package';
						borderWidth: 0.
			 PackagesIconsCache at: aPackage put: (IconicButtonStateHolder forNautilus: icon).
			^ icon ].

	(mcpackage notNil and: [(mcpackage name = aPackage name ) and: [ aPackage definedClasses isEmpty]])
		ifTrue: [ ^ PackagesIconsCache at: aPackage put: (self iconNamed: #monticelloPackageIcon) ].

	^ PackagesIconsCache at: aPackage put: (self iconNamed: #packageIcon)
]

{ #category : #private }
AbstractNautilusUI >> packageLabel [

	^'Packages'
]

{ #category : #'menu builder' }
AbstractNautilusUI >> packageMenuBuilder [

	^ PragmaMenuBuilder withAllPragmaKeywords: {self packageFixPragma. self packagePragma} model: self
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> packagePragma [
	^ 'nautilusGlobalPackageMenu'
]

{ #category : #'system announcements' }
AbstractNautilusUI >> packageRegistered: anAnnouncement [
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	((anAnnouncement package name beginsWith: 'ConfigurationOf') or: [ anAnnouncement package name beginsWith: 'BaselineOf' ])
		ifTrue:
			[ | configurationGroup |
			configurationGroup := self groupsManager groups detect: [ :group | group name = self model class configurationsGroupName ].
			self addPackages: anAnnouncement package asOrderedCollection inGroup: configurationGroup ].
	self updatePackageView
]

{ #category : #'system announcements' }
AbstractNautilusUI >> packageRenamed: anAnnouncement [
	window isDisplayed
		ifFalse: [ ^ self ].
	self updatePackageView
]

{ #category : #'system announcements' }
AbstractNautilusUI >> packageUnregistred: anAnnouncement [
	| package |
	package := anAnnouncement package.
	self selectedPackage = package
		ifTrue: [ self selectedPackage: nil ].
	self packageWidget selectPackage: package.
	window isDisplayed
		ifFalse: [ ^ self ].
	self updatePackageView
]

{ #category : #'widget package' }
AbstractNautilusUI >> packageWidget [
	self subclassResponsibility 
]

{ #category : #'menu builder' }
AbstractNautilusUI >> packagesMenu: aMenu shifted: aBoolean [ 
	^ aMenu addAllFrom: (self packageMenuBuilder menu)
]

{ #category : #private }
AbstractNautilusUI >> parentOfClass: aClass [ 
	^ aClass package.
]

{ #category : #private }
AbstractNautilusUI >> parentOfSelectedPackage [
	^ self selectedPackage node parentNode
]

{ #category : #'source code area' }
AbstractNautilusUI >> pendingText: aString [

	self updateSourceCodeAreaWith: aString.
	self sourceTextModel hasUnacceptedEdits: true
]

{ #category : #'menu-packages' }
AbstractNautilusUI >> promoteSelectedPackageTagAsPackage [
	self subclassResponsibility
]

{ #category : #'comment pane' }
AbstractNautilusUI >> promptForCancel [
	"Ask for the appropriate action to take when unaccepted contents
	would be overwritten."

	| choice |
	choice := UIManager default
		confirm: 'Contents has been modified.\What do you want to do?' translated withCRs
		trueChoice: 'Accept' translated
		falseChoice: 'Discard' translated
		cancelChoice: 'Cancel' translated
		default: nil.
	^ choice
	
]

{ #category : #NOCompletion }
AbstractNautilusUI >> receiverClass [

	^ self selectedClass
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> registerToAnnouncements [
	self registerToSystemAnnouncements.
	self registerToTestAnnouncements. 
	self registerToGroupAnnouncements. 
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> registerToGroupAnnouncements [
	GroupAnnouncer uniqueInstance weak
		when: AGroupHasBeenAdded send: #aGroupHasBeenAdded: to: self;
		when: AGroupHasBeenRemoved send: #aGroupHasBeenRemoved: to: self;
		when: AGroupHasBeenRenamed send: #aGroupHasBeenRenamed: to: self;
		when: AGroupContentHasBeenModified send: #aGroupContentHasBeenModified: to: self
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> registerToSystemAnnouncements [
	self browsedEnvironment packageOrganizer announcer weak
		when: RPackageRegistered send: #packageRegistered: to: self;
		when: RPackageUnregistered send: #packageUnregistred: to: self;
		when: RPackageRenamed send: #packageRenamed: to: self.
						
	SystemAnnouncer uniqueInstance weak
		when: MCPackageModified send: #mcPackageModified: to: self;
		when: MCWorkingCopyCreated send: #mcWorkingCopyCreated: to: self;
		when: MCWorkingCopyDeleted send: #mcWorkingCopyDeleted: to: self;
		when: ClassAdded send: #classAdded: to: self;
		when: ClassCommented send: #classCommented: to: self;
		when: ClassRecategorized send: #classRecategorized: to: self;
		when: ClassModifiedClassDefinition send: #classDefinitionModified: to: self;
		when: ClassParentRenamed send: #classParentRenamed: to: self;
		when: ClassRemoved send: #classRemoved: to: self;
		when: ClassRenamed send: #classRenamed: to: self;
		when: ClassReorganized send: #classReorganized: to: self;
		when: MethodAdded send: #methodAdded: to: self;
		when: MethodModified send: #methodModified: to: self;
		when: MethodRecategorized send: #methodRecategorized: to: self;
		when: MethodRemoved send: #methodRemoved: to: self;
		when: MetalinkChanged send: #metaLinkModified: to: self.

]

{ #category : #'announcement registration' }
AbstractNautilusUI >> registerToTestAnnouncements [
	| testCaseClass |
	testCaseClass := Smalltalk at: #TestCase ifAbsent:[^ self].
	testCaseClass historyAnnouncer weak
		when: (Smalltalk at: #TestCaseStarted) send: #testCaseStarted: to: self;
		when: (Smalltalk at: #TestSuiteEnded) send: #testRan: to: self.
]

{ #category : #'icon caches' }
AbstractNautilusUI >> removeClassFromMethodsIconsCache: aClass [

	self methodsIconsCache keysDo: [:method | 
		" for overrides "
		method methodClass = aClass
			ifTrue: [ 
				self methodsIconsCache removeKey: method ifAbsent: [ ]]].
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> removeClasses [
	"Remove the selected classes from the system, at interactive user request.  Make sure the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| result scroll |
	self okToChange ifFalse: [^ false].
	scroll := self classWidget vScrollValue.
	result := self removeClasses: (self selectedClasses collect: #theNonMetaClass).

	result
		ifTrue: [	
			self resetClassesListSelection.
			self selectedClass: nil ].
	self classWidget vScrollValue: scroll.
	^ result
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> removeEmptyCategories [

	self okToChange ifFalse: [^ self].
	(self methodsForCategory: self selectedCategory) 
		ifEmpty: [ self selectedCategory: nil ].
		
	self removeEmptyCategoriesFrom: self selectedClass.
	
	self updateCategoryAndMethodList.
]

{ #category : #group }
AbstractNautilusUI >> removeFromGroupPackages: aCollectionOfPackages [
	self parentOfSelectedPackage item removePackages: aCollectionOfPackages
]

{ #category : #'icon caches' }
AbstractNautilusUI >> removeFromMethodsIconsCache: aMethod [
	self methodsIconsCache removeKey: aMethod ifAbsent: [  ].

	"If aMethod is a TestCase test method or if it's tested by one, then
	clear the test method result and remove it and all its tested methods
	from the icon cache."
	aMethod asTestMethod
		ifNotNil: [ :testMethod |
			testMethod methodClass clearResultOfTestMethod: testMethod.
			self methodsIconsCache removeKey: testMethod ifAbsent: [  ].
			testMethod correspondingTestedMethods do: [ :each |
				self methodsIconsCache removeKey: each ifAbsent: [  ] ] ].
]

{ #category : #'icon caches' }
AbstractNautilusUI >> removeFromPackagesIconsCache: aPackage [

	PackagesIconsCache removeKey: aPackage ifAbsent: [].
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> removeGroups [

	self okToChange ifFalse: [ ^ self ].
	self packageWidget selectedGroups
		ifNotEmpty:[:groups |
			self packageWidget resetGroupsSelection.
			groups do: [:group | self groupsManager removeAGroup: group ].
			self selectedGroup: nil].
		
]

{ #category : #styling }
AbstractNautilusUI >> removeIconStyle [
	|conf|
	
	conf := RubConfigurationChange new.
	conf configurationBlock: [:text | 
		text segments copy do: #delete].  
	self sourceTextModel announce: conf.
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> removePackages [

	| names |
	self selectedPackages 
		ifNotEmpty: [:packages |
			names := packages collect: [:package | package name ].
			names := names joinUsing: String cr.
			(UIManager default confirm: ('Are you sure you want to delete the package(s) named ', String cr, names, ' and their classes ?'))
				ifTrue: [| scroll |
					packages do: [:package |
						package definedClasses do:[: classToRemove |
									classToRemove subclasses notEmpty
									ifTrue: [ | message |
										message := 'Are you certain that you
want to REMOVE the class ', classToRemove name, '
from the system?'.
										(self confirm: 'class has subclasses: ' , message)
										ifFalse: [^ nil]].
									classToRemove removeFromSystem ].
						self browsedEnvironment packageOrganizer unregisterPackage: package ].
					scroll := self packageWidget scrollValue y .
					self selectedPackage: nil.
					self updatePackageView.
					self packageWidget vScrollValue: scroll ]]
]

{ #category : #shortcuts }
AbstractNautilusUI >> removeShortcuts: groupSymbol from: aMorph [
	Nautilus useOldStyleKeys ifFalse: [ ^self ].
	
	(aMorph kmDispatcher includesKeymapCategory: groupSymbol)
		ifTrue: [ 
			aMorph 
				detachKeymapCategory: groupSymbol
				targetting: self ]

]

{ #category : #'menus behavior' }
AbstractNautilusUI >> renameCategory [
	^ self subclassResponsibility
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> renameClass [
	self okToChange ifFalse: [ ^ self ].
	self selectedClass ifNil: [ ^ self ].
	self basicRenameClass: self selectedClass theNonMetaClass
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> renameGroup [

	| group |
	group := self selectedGroup.
	group ifNil: [ ^ self ].
	[self groupsManager renameAGroup: group ]
		on: GroupAlreadyExists 
		do:[ :ex | self alertGroupExisting: ex groupName ].
	self updatePackageGroupAndClassList
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> renamePackage [

	self selectedPackages do: [ :package | self renamePackage: package ].
	self updatePackageView.
	
]

{ #category : #selection }
AbstractNautilusUI >> resetAllListSelections [
	self subclassResponsibility 
]

{ #category : #selection }
AbstractNautilusUI >> resetCategoriesList [
	self subclassResponsibility 
]

{ #category : #selection }
AbstractNautilusUI >> resetCategoriesListSelection [
	self subclassResponsibility 
]

{ #category : #selection }
AbstractNautilusUI >> resetClassesList [
	self subclassResponsibility 
]

{ #category : #selection }
AbstractNautilusUI >> resetClassesListSelection [
	self subclassResponsibility 
]

{ #category : #styling }
AbstractNautilusUI >> resetIconStyle [
	
	self removeIconStyle.
	self addIconStyle.

]

{ #category : #selection }
AbstractNautilusUI >> resetMethodsList [
	self subclassResponsibility 
]

{ #category : #selection }
AbstractNautilusUI >> resetMethodsListSelection [
	self subclassResponsibility 
]

{ #category : #selection }
AbstractNautilusUI >> resetPackageGroupsListSelection [
	self subclassResponsibility 
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowse [
	self selectedClass
		ifNil: [ self restrictedBrowsePackage ]
		ifNotNil: [ self restrictedBrowseClass ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseClass [
	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseClasses: {class} ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseClasses: classes [

	| newEnvironment |
	newEnvironment := self browsedEnvironment forClasses: (classes collect:#theNonMetaClass).
	self model class 
		openOnClass:self selectedClass 
		inEnvironment: newEnvironment 
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseGroup [

	self restrictedBrowseGroups: self packageWidget selectedGroups
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseGroups: aCollection [
	aCollection
		ifNotEmpty: [ | environment |
			environment := aCollection allButFirst
				inject: (aCollection first restrict: self browsedEnvironment)
				into: [ :env :group | env | (group restrict: self browsedEnvironment) ].
			self model class openOnGroup: aCollection first inEnvironment: environment ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowsePackage [

	self selectedPackages
		ifEmpty: [ ^ self ]
		ifNotEmpty: [ :packages |
			^ self restrictedBrowsePackages: packages ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowsePackageRegex [
	| regex |
	
	regex := self selectedPackages
		ifEmpty: [ '' ]
		ifNotEmpty: [ :packages | 
			'^',(packages first name splitOn: $-) first , '-.*$'].
		
	regex := UIManager default request: 'Browse restricted on Packages matching:' regex: regex .
	regex ifNil: [ ^ self ].
	self restrictedBrowsePackages: (self browsedEnvironment packages select: [ :package| regex matches: package name ] )
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowsePackages: packages [
	| newEnvironment |
	newEnvironment := self browsedEnvironment forPackages: packages.
	 self model class 
		openOnPackage: self selectedPackage inEnvironment: newEnvironment
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseSubclasses [

	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseClasses: class withAllSubclasses]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseSuperclasses [

	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseClasses: class withAllSuperclasses ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseTraitUsers [
	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseTraitUsers: class users ]
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> restrictedBrowseTraitUsers: users [

	| newEnvironment |
	users ifEmpty: [ ^ self ].
	newEnvironment := self browsedEnvironment forClasses: users.
	self model class 
		openOnClass: users anyOne 
		inEnvironment: newEnvironment 
]

{ #category : #'icons behavior' }
AbstractNautilusUI >> runClassTests [

	self runClassTests: self selectedClasses notifying: true
]

{ #category : #private }
AbstractNautilusUI >> runClassTests: aCollection notifying: aBoolean [

	(aCollection allSatisfy: [:each | each isTestCase ]) ifFalse: [ ^ self ].
	aCollection do: [:class | self runTestsOfClass: class notifying: aBoolean ]
]

{ #category : #private }
AbstractNautilusUI >> runPackagesTestsNotifying: aBoolean [
	self selectedPackages
		ifNotNil: [ :packages | 
			| classes label |
			label := packages size > 1
				ifTrue: [ 'Packages: ' ]
				ifFalse: [ 'Package: ' ].
			classes := packages
				flatCollect: [ :package | package definedClasses ].
			classes := classes select: [ :class | class isTestCase ].
			self runClassTests: classes notifying: false.
			aBoolean
				ifTrue:
					[ self notifyTestResultFrom: packages classes: classes labeled: label ] ]
]

{ #category : #'icons behavior' }
AbstractNautilusUI >> runTestForAMethod: aMethod notifying: anObject [

	^ self subclassResponsibility
]

{ #category : #private }
AbstractNautilusUI >> runTestForMethods: aCollection notifying: aBoolean [

	aCollection
		do: [ :each | 
			each isTestMethod
				ifTrue: [ self runTestForAMethod: each notifying: aBoolean ]]
]

{ #category : #private }
AbstractNautilusUI >> runTestsOfClass: aClass notifying: aBoolean [
	| color result |
	aClass resetHistory.
	result := aClass suite run.
	result updateResultsInHistory.
	
	aBoolean ifFalse: [ ^self ].
	color := Color gray.
	result hasPassed ifTrue: [ color := self theme successBackgroundColor ].
	result hasFailures ifTrue: [ color := self theme warningBackgroundColor ].
	result hasErrors ifTrue: [ color := self theme dangerBackgroundColor ].
			
	self 
		notifyTitle: aClass name 
		contents: result printString
		color: color
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> runTestsOfGroups: aCollection notifying: aBoolean [
	
	| classes label |
	label := 'Group: '.
	aCollection size >1 ifTrue: [ label := 'Groups: '].
	classes := aCollection flatCollect:[:each | each elements ].
	classes := classes select: [:class | class isTestCase ].
	self runClassTests: classes notifying: false.
	aBoolean
		ifFalse: [ self
					notifyTitle: 'Test Finished'
					contents: label,((aCollection collect: #name) joinUsing: ', ') ]
]

{ #category : #selection }
AbstractNautilusUI >> selectNextMethod [
	self subclassResponsibility 
]

{ #category : #'SmalltalkEditor compatibility' }
AbstractNautilusUI >> selectedBehavior [

	^ self selectedClass
]

{ #category : #'widget category' }
AbstractNautilusUI >> selectedCategory [

	^ self subclassResponsibility
]

{ #category : #'widget category' }
AbstractNautilusUI >> selectedCategory: anObject [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractNautilusUI >> selectedClass [
	| class |
	class := self model selectedClass.
	(self model selectedCategory isNil and: [ self model selectedMethod isNil ])
		ifTrue: [ self setStylerClass: nil ]
		ifFalse: [ self setStylerClass: class ].
	^ class
]

{ #category : #accessing }
AbstractNautilusUI >> selectedClass: aClass [
	self okToChangeSourceAndComment
		ifFalse: [ ^ self ].
	self resetClassesListSelection.
	aClass ifNotNil: [ self giveFocusTo: self classWidget ].
	self selectedClassWithoutChangingSelection: aClass
]

{ #category : #'source code area' }
AbstractNautilusUI >> selectedClassDescription [

	^ self selectedClass definitionForNautilus

]

{ #category : #NOCompletion }
AbstractNautilusUI >> selectedClassOrMetaClass [
	self selectedCategory ifNil: [ self selectedMethod ifNil: [ ^ nil ] ].
	^ self selectedClass
]

{ #category : #accessing }
AbstractNautilusUI >> selectedClassWithoutChangingSelection: aClass [ 
	^ self subclassResponsibility
]

{ #category : #private }
AbstractNautilusUI >> selectedClasses [
	^ self classWidget selectedClasses
]

{ #category : #accessing }
AbstractNautilusUI >> selectedGroup [
	^ self model selectedGroup
]

{ #category : #accessing }
AbstractNautilusUI >> selectedGroupWithoutChangingSelection: anObject [

	self okToChange
		ifTrue: [
			anObject ifNil: [ self packageWidget resetGroupsSelection ].
			self model selectedGroup: anObject.
			self packageWidget selectGroup: anObject.
			self selectedClass: nil.
			self updateClassView ]
]

{ #category : #'widget method' }
AbstractNautilusUI >> selectedMethod [

	^ self subclassResponsibility
]

{ #category : #'widget method' }
AbstractNautilusUI >> selectedMethod: aMethod [

	^ self subclassResponsibility
]

{ #category : #'source code area' }
AbstractNautilusUI >> selectedMethodSource [
	self selectedMethod ifNil: [ ^ '' ].
	^ self selectedMethod sourceCode
]

{ #category : #accessing }
AbstractNautilusUI >> selectedPackage [
	^ self model selectedPackage
]

{ #category : #accessing }
AbstractNautilusUI >> selectedPackage: aPackage [
	"Force package selection, not used by the lists "
	self okToChange
		ifTrue: [
			acceptor := ClassDefinitionAcceptor model: self.
			self resetPackageGroupsListSelection.
			self packageWidget selectPackage: aPackage.		
			aPackage ifNotNil: [ self giveFocusTo: self packageWidget ].		
			self selectedPackageWithoutChangingSelection: aPackage ]
]

{ #category : #accessing }
AbstractNautilusUI >> selectedPackageGroup [
	"give selected element from package/group pane, whatever is selected"
	^ self packageWidget selectedPackageGroup
]

{ #category : #accessing }
AbstractNautilusUI >> selectedPackageWithoutChangingSelection: aPackage [
	self okToChange
		ifTrue:
			[ | class |
			class := self selectedClass.
			(self showHierarchy not and: [ class notNil and: [ class package ~= aPackage ] ])
				ifTrue: [ self selectedClass: nil ].
			self model
				package: aPackage
				class: self selectedClass.
			self packageWidget selectPackage: aPackage.
			self updateClassView.
			self updateCategoryAndMethodList ]
]

{ #category : #accessing }
AbstractNautilusUI >> selectedPackageWithoutChangingSelectionInternally: aPackage [
	| class |
	class := self selectedClass.
	(self showHierarchy not and: [ class notNil and: [ class package ~= aPackage ] ])
		ifTrue: [ self selectedClass: nil ].
	self packageWidget selectPackage: aPackage.
	aPackage ifNotNil: [ self giveFocusTo: self packageWidget ].
	self updateClassView.
	self updateCategoryAndMethodList
]

{ #category : #private }
AbstractNautilusUI >> selectedPackages [
	^ self packageWidget selectedPackages
]

{ #category : #accessing }
AbstractNautilusUI >> selectionInterval [
	^ sourceCodeSelectionInterval
]

{ #category : #accessing }
AbstractNautilusUI >> selectionInterval: anInterval [
	sourceCodeSelectionInterval := anInterval
]

{ #category : #shortcuts }
AbstractNautilusUI >> setCommentShorcutsTo: aList [

	aList attachKeymapCategory: #NautilusCommentShortcuts targetting: self
]

{ #category : #shortcuts }
AbstractNautilusUI >> setGroupShorcutsTo: aList [
	aList attachKeymapCategory: #NautilusGroupShortcuts targetting: self
]

{ #category : #shortcuts }
AbstractNautilusUI >> setShortcuts: groupSymbol to: aMorph [
	aMorph 
		attachKeymapCategory: (	Nautilus useOldStyleKeys 
			ifTrue: [ groupSymbol ]
			ifFalse: [ #NautilusGlobalShortcuts ])
		targetting: self

]

{ #category : #shortcuts }
AbstractNautilusUI >> setSourceCodeShorcutsTo: aList [

	aList attachKeymapCategory: #NautilusSourceCodeShortcuts targetting: self
]

{ #category : #'source code area' }
AbstractNautilusUI >> setStylerClass: aClass [
	self sourceTextModel reconfigureViewWith: [ :scrolledText | scrolledText textArea classOrMetaClass: aClass ]
]

{ #category : #displaying }
AbstractNautilusUI >> setWindowTitle [

	| title |
	shouldUpdateTitle ifFalse: [ ^ self ].
	title := self selectedClass 
				ifNil: [ self selectedPackage
							ifNil: [ self title ]
							ifNotNil: [: p | p name ]]
				ifNotNil: [:class |
					self selectedMethod
						ifNil: [ class name asString ]
						ifNotNil: [:method |
							String streamContents: [:s | s << method methodClass name asString<< '>>#'<< method selector ]]].
	self browsedEnvironment isSystem ifFalse: [ title := String streamContents: [:s | s << '--- ' << title << ' ---']].
	window title: title.
]

{ #category : #'scroll bar events' }
AbstractNautilusUI >> shiftedTextPaneMenuRequest [
]

{ #category : #accessing }
AbstractNautilusUI >> showComment [

	^ self model showComment
]

{ #category : #accessing }
AbstractNautilusUI >> showComment: aBoolean [
	
	self okToChangeComment
		ifFalse: [ ^ self ].
	self model showComment: aBoolean.
	
	self updateCodePanel
]

{ #category : #accessing }
AbstractNautilusUI >> showHierarchy [

	^ self model showHierarchy
]

{ #category : #accessing }
AbstractNautilusUI >> showInstance [

	^ self model showInstance
]

{ #category : #accessing }
AbstractNautilusUI >> showInstance: aBoolean [

	self okToChangeSourceAndComment
		ifTrue:[
			self model showInstance: aBoolean.
			self resetClassesListSelection.
			self selectedClass 
				ifNotNil: [:class |
					aBoolean 
						ifTrue: [self selectedClass: class theNonMetaClass.
								acceptor := ClassOrMethodDefinitionAcceptor model: self ]
						ifFalse: [self selectedClass: class theMetaClass.
								acceptor := ClassDefinitionAcceptor model: self ]].
			self updateCategoryAndMethodList.
			self changed: #instanceButtonState.
			self changed: #instanceButtonLabel ]
]

{ #category : #'source code area' }
AbstractNautilusUI >> showScriptCode: anAnnouncement [
	self setStylerClass: nil.
	self sourceTextModel setText: self sourceCode.
	self sourceTextModel clearUndoManager.
	self sourceTextModel hasUnacceptedEdits: false.
]

{ #category : #'source code area' }
AbstractNautilusUI >> showSourceCode: anAnnouncement [
	| formatter |
	formatter := RBProgramNode formatterClass.
	self sourceTextModel setText: self sourceCode.
	formatter formatAsYouReadPolicy
		ifTrue: [ 
			self sourceTextModel
				formatSourceCodeInView].
	self sourceTextModel clearUndoManager.
	self sourceTextModel hasUnacceptedEdits: false
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> showUnreferencedClassVars [
	"Search for all class variables known to the selected class, and put up a 
	list of those that have no references anywhere in the system. The 
	search includes superclasses, so that you don't need to navigate your 
	way to the class that defines each class variable in order to determine 
	whether it is unreferenced"
	self showUnreferencedClassVarsOf: self selectedClass.
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> showUnreferencedInstVars [
	"Search for all instance variables known to the selected class, and put up a list of those that have no references anywhere in the system.  The search includes superclasses, so that you don't need to navigate your way to the class that defines each inst variable in order to determine whether it is unreferenced"

	self showUnreferencedInstVarsOf: self selectedClass
]

{ #category : #accessing }
AbstractNautilusUI >> sortHierarchically [

	^ self model sortHierarchically
]

{ #category : #'source code area' }
AbstractNautilusUI >> sourceCode [

	^self selectedClass 
		ifNil: [ self defaultClassDescriptor]
		ifNotNil: [:class | self selectedMethod 
			ifNil: [ self selectedCategory 
					ifNotNil: [ 
							class sourceCodeTemplate]
					ifNil: [ self selectedClassDescription]]
			ifNotNil: [ 
					self selectedMethodSource]].
		
]

{ #category : #'browser compatibility' }
AbstractNautilusUI >> sourceCode: aText [ 

	self okToChange
		ifTrue: [ 
			self updateSourceCodeAreaWith: aText.
			self giveFocusToSourceCodeArea ]
]

{ #category : #accessing }
AbstractNautilusUI >> sourceCodeContainer [
	^ sourceCodeContainer
]

{ #category : #'menu builder' }
AbstractNautilusUI >> sourceCodeMenu: aMenu shifted: shifted [
	| menu |
	menu := (PragmaMenuBuilder
		pragmaKeyword: self sourceTextWidget editingModeClass menuKeyword
		model: self sourceTextWidget) menu.
	^ aMenu
		addAllFrom: self sourceCodeMenuBuilder menu;
		addLine;
		addAllFrom: menu;
		yourself
]

{ #category : #'menu builder' }
AbstractNautilusUI >> sourceCodeMenuBuilder [

	^ PragmaMenuBuilder pragmaKeyword: self sourceCodePragma model: self
]

{ #category : #accessing }
AbstractNautilusUI >> sourceCodePanel [
	^ sourceCodePanel
]

{ #category : #'menu pragmas' }
AbstractNautilusUI >> sourceCodePragma [
	^ 'nautilusGlobalSourceCodeMenu'
]

{ #category : #accessing }
AbstractNautilusUI >> sourceTextModel [
	^ sourceTextModel ifNil: [ sourceTextModel := RubScrolledTextModel new interactionModel: self ].
]

{ #category : #accessing }
AbstractNautilusUI >> sourceTextWidget [
	^ sourceTextWidget 
]

{ #category : #private }
AbstractNautilusUI >> systemNavigation [

	^ SystemNavigation new 
			browsedEnvironment: self browsedEnvironment;
			yourself
]

{ #category : #'system announcements' }
AbstractNautilusUI >> testCaseStarted: anAnnouncement [
	| class selector |
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	class := anAnnouncement testCase class.
	selector := anAnnouncement testSelector.
	self selectedClass = class
		ifTrue: [| method icon |
			icon := self iconNamed: #testNotRunIcon.
			method := class methodNamed: selector.
			self methodWidget 
				setIcon: icon for: method;
				updateMethodList ]
]

{ #category : #'system announcements' }
AbstractNautilusUI >> testRan: anAnnouncement [
	| testCaseClass classTested |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	"testResult actually returns the TestCase class ran"
	testCaseClass := anAnnouncement testResult.
	classTested := testCaseClass correspondingClassTested.
	(self classWidget getClassesList
		includesAnyOf:
			{testCaseClass.
			classTested.
			classTested class})
		ifTrue: [ self
				removeClassFromMethodsIconsCache: testCaseClass;
				removeClassFromMethodsIconsCache: classTested;
				removeClassFromMethodsIconsCache: classTested class;
				updateClassView;
				updateCategoryAndMethodList ]
]

{ #category : #private }
AbstractNautilusUI >> title [
	^ 'Nautilus - System Browser'
]

{ #category : #accessing }
AbstractNautilusUI >> title: aString [

	shouldUpdateTitle := false.
	window title: aString
]

{ #category : #'menus behavior' }
AbstractNautilusUI >> toggleBytecodes [
	self selectedMethod ifNil: [ ^ self ].
	self
		updateSourceCodeAreaWith:
			(self sourceTextModel getString startsWithDigit
				ifTrue: [ self sourceTextModel getString ]
				ifFalse: [ self selectedMethod symbolic ])
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> toggleShowComment [
	self showComment: self showComment not.
	self changed: #commentButtonState
	
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> toggleShowFullComment [

	self showComment ifFalse: [ ^ self ].
	sourceCodePanel removeAllMorphs.
	sourceCodePanel 
		addMorph: commentWidget 
		fullFrame: LayoutFrame identity.

]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> toggleShowHierarchy [

	self showHierarchy: self showHierarchy not.
]

{ #category : #'buttons behavior' }
AbstractNautilusUI >> toggleShowInstance [

	self showInstance: self showInstance not.
	self changed: #instanceButtonState.
	self categoryWidget label: self categoriesLabel.
	self methodWidget label: self methodsLabel.
	instanceButtonWidget helpText: (self instanceButtonHelpTextFor: self instanceButtonState).
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> unregisterAPlugin: aPlugin [

	self announcer unsubscribe: aPlugin
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> unregisterAllPlugins [

	self announcer subscriptions reset
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> unregisterToAnnouncements [
	self unregisterToSystemAnnouncements.
	self unregisterToTestAnnouncements. 
	self unregisterToGroupAnnouncements. 
]

{ #category : #'announcement registration' }
AbstractNautilusUI >> unregisterToGroupAnnouncements [
	GroupAnnouncer uniqueInstance unsubscribe: self.

]

{ #category : #'announcement registration' }
AbstractNautilusUI >> unregisterToSystemAnnouncements [
	self browsedEnvironment packageOrganizer announcer
	     unsubscribe: self.
	SystemAnnouncer uniqueInstance unsubscribe: self.

]

{ #category : #'announcement registration' }
AbstractNautilusUI >> unregisterToTestAnnouncements [
	| testCaseClass |
	testCaseClass := Smalltalk at: #TestCase ifAbsent:[^ self].
	testCaseClass historyAnnouncer unsubscribe: self.

]

{ #category : #updating }
AbstractNautilusUI >> updateCategoryAndMethodList [
	"abstractly defined here to silence a critic in #testRan:"
	self subclassResponsibility
]

{ #category : #updating }
AbstractNautilusUI >> updateCategoryAndMethodList: aMethodOrNil [
	"abstractly defined here because #updateCategoryAndMethodList is too"
	self subclassResponsibility
]

{ #category : #private }
AbstractNautilusUI >> updateClassView [

	self resetClassesList.
	self changed: #getClassItem: 
]

{ #category : #'code panel' }
AbstractNautilusUI >> updateCodePanel [
	sourceCodePanel removeAllMorphs.
	self model showComment
		ifTrue: [ self buildCodePanelWithComment ]
		ifFalse: [ self buildCodePanelWithoutComment ]
]

{ #category : #private }
AbstractNautilusUI >> updateGroupView [

	self changed: #getPackageItem:
]

{ #category : #'comment pane' }
AbstractNautilusUI >> updatePackageComment [
	self selectedPackage packageComment = commentTextModel getText asString
		ifTrue: [ ^ self ].
	self selectedPackage
		packageComment: commentTextModel getText asString printString
]

{ #category : #private }
AbstractNautilusUI >> updatePackageGroupAndClassList [

	self resetClassesList.
	self changed: #getPackageItem:.
	self changed: #getClassItem:
]

{ #category : #private }
AbstractNautilusUI >> updatePackageViewAndMove [
			
	self changed: #getPackageItem:
]

{ #category : #private }
AbstractNautilusUI >> updatePackageViewContent [

	| vScrollValue |
	vScrollValue := self packageWidget scrollValue y.
	self changed: #getPackageItem:.
	self packageWidget vScrollValue: vScrollValue
]

{ #category : #updating }
AbstractNautilusUI >> updateSourceCodeAreaWith: aText [
	self sourceTextModel setText: aText.
	self sourceTextModel hasUnacceptedEdits: false.
	
]

{ #category : #'button-variables' }
AbstractNautilusUI >> variablesMenuEntry: aClass items: aCollection in: aMenu using: aBlock [
	| menu |
	aCollection ifEmpty: [ ^ self ].
	menu := aMenu.
	aCollection size < 7
		ifTrue: [ menu
				addLine;
				addMorphBack: (aClass name asStringMorph color: Color gray).
			aCollection sorted
				do: [ :variable | aBlock value: variable value: aClass value: menu ] ]
		ifFalse: [ | groupsSize groups sorted leftover |
			groupsSize := 20.
			leftover := aCollection size \\ groupsSize.
			sorted := aCollection sorted.
			menu addLine.
			groups := (sorted
				groupsOf: groupsSize
				atATimeCollect: [ :each | each ]) asOrderedCollection.
			leftover > 0
				ifTrue: [ groups add: (sorted last: leftover) ].
			groups
				withIndexDo: [ :group :groupIndex | 
					| itemName firstLetter lastLetter |
					firstLetter := (group first, ' ') first asString.
					lastLetter := (group last, ' ') first asString.
					itemName :=  groups size > 1 
						ifTrue: [ aClass name, ' (', firstLetter, '-', lastLetter, ')' ]
						ifFalse: [ aClass name ].					
					menu := MenuMorph new
						defaultTarget: aMenu defaultTarget;
						color: aMenu color;
						yourself.
					aMenu
						add: itemName subMenu: menu.
					aMenu lastItem emphasis: 1.
					group
						do: [ :variable | aBlock value: variable value: aClass value: menu ] ] ]
]

{ #category : #'drag and drop' }
AbstractNautilusUI >> wantsDroppedMorph: transferMorph event: anEvent inMorph: destinationLM [
	"We are only interested in TransferMorphs as wrappers for             
	informations. If their content is really interesting for us, will             
	determined later in >>acceptDroppingMorph:event:."

	| srcType dstType |
	"only want drops on lists (not, for example, on pluggable texts)"
	(destinationLM isKindOf: AbstractWidget listClass)
		ifFalse: [ ^ false ].
	srcType := transferMorph dragTransferType.
	dstType := self dragTransferTypeForMorph: destinationLM.
	srcType == #getMethodItem:
		ifTrue:
			[ ^ dstType == #getCategoryItem: or: [ dstType == #getClassItem: ] ].
	srcType == #getCategoryItem:
		ifTrue: [ ^ dstType == #getClassItem: ].
	(srcType == #getClassItem: and: [ dstType = #getPackageItem: ])
		ifTrue:
			[ 
			(destinationLM potentialDropItem isKindOf: AbstractWidget listClass)
				ifTrue: [ ^ true ] ]
		ifFalse: [ ^ false ].
	^ false
]

{ #category : #'comment pane' }
AbstractNautilusUI >> whenExtendCommentSwitch: anAnnouncement [
	"when you press the comment button."
	self fullDisplayedComment
		ifFalse: [ self toggleShowFullComment ] 
		ifTrue: [ self buildCodePanelWithCommentOnRight ].
	self fullDisplayedComment: self fullDisplayedComment not.
]

{ #category : #'source text events' }
AbstractNautilusUI >> whenTextAcceptedInView: anAnnouncement [

	self compileSource: self sourceTextModel getString notifying: sourceTextWidget
]

{ #category : #'source text events' }
AbstractNautilusUI >> whenTextAreaSelectionChanged: aSelectionChangedAnnounce [
	self selectionInterval: aSelectionChangedAnnounce morph textArea selectionInterval.
	contentSelection :=  aSelectionChangedAnnounce morph textArea selection
]

{ #category : #'source text events' }
AbstractNautilusUI >> whenTextSegmentIconsClicked: anEvent [
	
	| aNode |

	(self canInstallMethodBreakpoint )
		ifFalse: [ ^ self ].

	aNode := self breakpointInstallationNodeFor: anEvent cursorPoint.

	self breakOn: aNode	
]

{ #category : #'source text events' }
AbstractNautilusUI >> whenTextSegmentIconsMouseUp: anEvent [

	| aNode|

	(anEvent yellowButtonChanged and: [self canInstallMethodBreakpoint ])
		ifFalse: [ ^ self ].
	
	aNode := self breakpointInstallationNodeFor: anEvent cursorPoint.
	
	self openBreakpointMenuFor: aNode.
]

{ #category : #'comment pane' }
AbstractNautilusUI >> whenTextUpdatedInCommentPane: anAnnouncement [
	self selectedClass
		ifNotNil: [ :class | 
			commentTextModel getText = class classCommentBlank
				ifFalse: [ class comment: commentTextModel getText stamp: Author changeStamp ] ]
		ifNil: [ self packageCommentEmptyOrUnchanged ifTrue: [ ^ self ]
			ifFalse:[ 
			(self selectedPackage isNotNil and:[ self selectedPackage isGroup not])
				ifTrue: [ self updatePackageComment] ] ]
]

{ #category : #accessing }
AbstractNautilusUI >> window [
	^ window
]

{ #category : #accessing }
AbstractNautilusUI >> window: anObject [
	window := anObject
]

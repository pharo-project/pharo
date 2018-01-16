"
A UI for an instance of Nautilus
"
Class {
	#name : #NautilusUI,
	#superclass : #AbstractNautilusUI,
	#instVars : [
		'categoryWidget',
		'methodWidget',
		'classWidget',
		'packageWidget',
		'packagePatternString',
		'packagePattern',
		'searchWidget'
	],
	#category : #Nautilus
}

{ #category : #keymapping }
NautilusUI class >> buildHistoryShortcutsOn: aBuilder [
	<keymap>

	(aBuilder shortcut: #previous)
		category: #NautilusGlobalShortcuts
		default: $[ meta shift
		do: [:target| target previous ]
		description: 'Go back in the navigation history.'.
		
	(aBuilder shortcut: #next)
		category: #NautilusGlobalShortcuts
		default: $] meta shift
		do: [:target| target next ]
		description: 'Go forward in the navigation history'.
	
]

{ #category : #'widget method' }
NautilusUI class >> notifyTitle: title contents: contents color: aColor [

	 GrowlMorph 
		openWithLabel: title
		contents: contents
		backgroundColor: aColor
		labelColor: Color black
				
	
]

{ #category : #menu }
NautilusUI class >> packagesFixMenu: aBuilder [
	<contextMenu>
	<nautilusGlobalPackageTreeFixMenu>
	
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
		help: 'Add a package'.
		
	(aBuilder item: #'Add tag...')
		action: [ target addClassCategory ];
		order: 210;
		help: 'Add a tag/category in package';
		withSeparatorAfter.
		
	(aBuilder item: #'Browse full')
		keyText: 'b, f' if: Nautilus useOldStyleKeys not;
		keyText: 'b' if: Nautilus useOldStyleKeys;
		action: [target fullBrowse];
		order: 999;
		help: 'Open the same browser'.
]

{ #category : #'menus behavior' }
NautilusUI class >> runTestForAMethod: aMethod notifying: aBoolean [
	| testMethod testClass testResult color |
	(testMethod := aMethod asTestMethod) ifNil: [ ^ self ].
	testClass := testMethod methodClass.
	testResult := testClass debug: testMethod selector.
	aBoolean
		ifFalse: [ ^ self ].
	color := Color gray.
	testMethod isPassedTest
		ifTrue: [ color := self theme successBackgroundColor ].
	testMethod isFailedTest
		ifTrue: [ color := self theme warningBackgroundColor ].
	testMethod isErrorTest
		ifTrue: [ color := self theme dangerBackgroundColor ].
	self
		notifyTitle: 'Test Finished'
		contents: 'Method: ' , testMethod methodClass asString , '>>#' , testMethod selector asString
		color: color
]

{ #category : #'menus behavior' }
NautilusUI class >> toggleBreakpointIn: aMethod [
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint
		ifTrue: [ 
				aMethod ast nodesWithLinks do:[:link | Breakpoint removeFrom: link ]]
		ifFalse: [ Breakpoint new node: aMethod ast; always; install].
		
	SystemAnnouncer uniqueInstance methodChangedFrom: aMethod to: aMethod oldProtocol: aMethod protocol
]

{ #category : #'menus behavior' }
NautilusUI >> addCategory [
	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"
	| newName |
	
	self okToChange ifFalse: [^ self].
	self selectedClass ifNil: [^ self].

	newName := self 
		addCategoryIn: self selectedClass 
		before: self selectedCategory.
	self resetCategoriesListSelection.
	categoryWidget selectProtocol: newName.
	self selectedCategory: newName.
	newName ifNil: [ ^ self ].
	self 
		updateCategoryAndMethodList;
		giveFocusToSourceCodeArea.
]

{ #category : #'menus behavior' }
NautilusUI >> addClassCategory [ 
	| package |

	package := self addClassCategoryBasedOn: self selectedPackage.
	package ifNotNil: [
		self selectedPackage: package.
		self updatePackageView ]
]

{ #category : #private }
NautilusUI >> addClassCategoryBasedOn: aSelection [
	| name package packageTag |

	packageTag := nil.
	package := aSelection package.
	name := UIManager default 
		request: ('New category: {1}-' format: { package name }) 
		initialAnswer: '' 
		title: 'Create a new category'.
	
	[ (self isValidPackageName: name)
		ifTrue: [ packageTag := package addClassTag: name ]
		ifFalse: [ self alertInvalidPackageName: name ] ]
	on: RPackageConflictError 
	do: [ :e | UIManager default inform: e messageText, '. You need to use promote/demote options instead.' ].
	
	^ packageTag
]

{ #category : #class }
NautilusUI >> addClassIn: aPackage [
	super addClassIn: aPackage asNautilusSelection item.
]

{ #category : #package }
NautilusUI >> addPackageBasedOn: aPackage [
	[ ^ super addPackageBasedOn: aPackage ]
	on: RPackageConflictError 
	do: [ :e | UIManager default inform: e messageText, '. You need to use promote/demote options instead.' ].
	
	^ nil
]

{ #category : #package }
NautilusUI >> addPackagesAsGroup: aCollection [
	| group |
	
	aCollection isEmptyOrNil ifTrue: [ ^ nil ].
	
	group := super addPackagesAsGroup: aCollection.
	self updatePackageView.
	^ group
]

{ #category : #class }
NautilusUI >> addTraitIn: aPackage [
	super addTraitIn: aPackage asNautilusSelection item.
]

{ #category : #package }
NautilusUI >> alertCategoryName: tagName exitstsAlreadyIn: package [
	| errorMessage |
	errorMessage := String
		streamContents: [ :stream | stream << 'Category named ' << tagName << ' already exists in package ' << package name ].
	UIManager default inform: errorMessage
]

{ #category : #'widget method' }
NautilusUI >> allLabel [

	^ AllProtocol defaultName
]

{ #category : #private }
NautilusUI >> basicRemoveClasses: aCollection [ 
		aCollection do: #removeFromSystem.
]

{ #category : #refactorings }
NautilusUI >> basicRenameClass: aClass [
	" Override to set the class rename with RB by default "

	self renameClassWithRefactoringEngine: aClass
]

{ #category : #private }
NautilusUI >> basicRenamePackage: aPackage [ 

	| newName |
	newName := UIManager default 
		request: 'New name of the package' 
		initialAnswer: aPackage name 
		title: 'Rename a package'.
	newName = aPackage name ifTrue: [ ^ self ].
	
	(self isValidPackageName: newName)
		ifTrue: [ aPackage renameTo: newName ]
		ifFalse: [ self alertInvalidPackageName:newName ].
]

{ #category : #accessing }
NautilusUI >> basicSelectPackage: aPackage [
	| selection |
			
	selection := aPackage asNautilusSelection.
	self giveFocusTo: self packageWidget.
	self selectedPackageWithoutChangingSelection: selection.
	self listSelectItem: selection.
]

{ #category : #private }
NautilusUI >> basicShowHierarchy [
	"Prepares the class panel to show hierarchies"
	
	self selectedClass ifNil: [ ^ self ].	"Since the double clic comes after a simple clic and simple clic deselects selected item, we are forced to have this guard. A alternative would be to change the simple clic but the side effects are too big"
	self resetClassesListSelection.
	self classWidget selectClass: self selectedClass.
	self resetClassesList.	
]

{ #category : #'widget category' }
NautilusUI >> buildCategoryWidget [
	| categoriesList |
	
	categoriesList := categoryWidget buildCategoriesList.	
	
	self 
		setShortcuts: #NautilusProtocolShortcuts 
		to: categoriesList. 
	
	^categoriesList
]

{ #category : #'widget class' }
NautilusUI >> buildClassWidget [
	| classesList |
	classesList := classWidget buildClassesList.
	self setShortcuts: #NautilusClassShortcuts to: classesList.
	^ classesList
]

{ #category : #'build ui lists' }
NautilusUI >> buildFirstColumn: aWindow [
	^ PanelMorph new
		changeProportionalLayout;
		addMorph: self buildPackageSearch
			fullFrame:
			(LayoutFrame identity
				bottomFraction: 0;
				bottomOffset: StandardFonts defaultFont pixelSize + 10;
				leftOffset: -1;
				rightOffset: 1);
		addMorph: self buildPackageWidget
			fullFrame: (LayoutFrame identity topOffset: StandardFonts defaultFont pixelSize + 14);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]

{ #category : #'widget method' }
NautilusUI >> buildMethodWidget [
	| methodsList |
	
	methodsList := methodWidget buildMethodsList.	
	
	self 
		setShortcuts: #NautilusMethodShortcuts 
		to: methodsList. 
	
	^methodsList	

]

{ #category : #'package filter' }
NautilusUI >> buildPackageSearch [
	"It is an search dialog which filters package list"
	| textMorph |
	
	searchWidget := SearchMorph new
		model: self;
		updateSelector: #packageSearchUpdate:;
		ghostText: 'Type: Pkg1|^Pkg2|Pk.*Core$';
		list: SharedPackageFilterPatternsHolder instance;
		useSelectionIndex: false;
		setIndexSelector: #packageSearchUpdate:;
		yourself.
		
	textMorph := searchWidget contentMorph textMorph.	
	self setShortcuts: #NautilusGlobalShortcuts to: textMorph.
	textMorph bindKeyCombination: Character tab asKeyCombination toAction: [ textMorph navigateFocusForward ].
	
	
	^ searchWidget
]

{ #category : #'widget package' }
NautilusUI >> buildPackageWidget [
	| packagesList |
	packagesList := packageWidget buildPackagesList.
	self setShortcuts: #NautilusPackageShortcuts to: packagesList.
	^ packagesList
]

{ #category : #'test creation' }
NautilusUI >> buildTestPackageNameFrom:aClass [
	^ aClass package categoryName asString, '-Tests' 
]

{ #category : #'widget method' }
NautilusUI >> buildTestSelectorFor: aMethod [

	^ String streamContents: [:s || capitalize |
		capitalize := true.
		s << 'test'.
		aMethod selector do: [:c |
			c = $:
				ifTrue: [ capitalize := true ]
				ifFalse: [ capitalize
							ifTrue: [ 
								capitalize := false.
								s << c asUppercase. ]
							ifFalse:[ s << c ]]]]
			
]

{ #category : #'widget category' }
NautilusUI >> categoriesLabel [

	^ categoryWidget categoriesLabel
]

{ #category : #'widget category' }
NautilusUI >> categoriesMenu: aMenu shifted: b [

	^ aMenu becomeForward: (self categoryMenuBuilder menu)
]

{ #category : #'widget category' }
NautilusUI >> categoriesSelection [

	^ categoryWidget categoriesSelection
]

{ #category : #'menus behavior' }
NautilusUI >> categorizeMethod [

	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"
	| aCollection aClass |
	self okToChange ifFalse: [^ self].
	(aCollection := self selectedMethods) ifEmpty: [^ self].
	(aClass := self selectedClass) ifNil: [^self].	

	self categorizeMethods: aCollection of: aClass from: aCollection first category.

	self updateCategoryAndMethodList.
]

{ #category : #package }
NautilusUI >> category: aClassTag existsIn: aPackage [
	aPackage classTagNamed: aClassTag ifPresent:[^ true].
	^ false.
]

{ #category : #'widget category' }
NautilusUI >> categorySelectionChanged [

	self resetMethodsList
	methodWidget updateMethodList.
]

{ #category : #'widget category' }
NautilusUI >> categoryWidget [

	^ categoryWidget
]

{ #category : #'package filter' }
NautilusUI >> class: aClass matchPackagePatternString: aString [ 
	| pattern |
	pattern := self createPackagePattern: aString.
	^ self package: aClass package matchPackagePattern: pattern
]

{ #category : #'system announcements' }
NautilusUI >> classAdded: anAnnouncement [ 
	| class |
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	class := anAnnouncement classAdded.
	((self parentOfClass: class) nodePath 
		anySatisfy: [ :node | node item = class package ]) 
		ifTrue: [ 
			self updateClassView.
			self removeFromPackagesIconsCache: class package ]
			
]

{ #category : #'widget class' }
NautilusUI >> classWidget [

	^ classWidget
]

{ #category : #private }
NautilusUI >> clearPackageSelections [
	self resetClassesList.
	self resetPackageGroupsListSelection.

]

{ #category : #'menus behavior' }
NautilusUI >> compareTwoMethods [
	
	(DiffPresenter new
		showOptions: true;
		leftText: self selectedMethods first sourceCode;
		rightText: self selectedMethods second sourceCode;
		contextClass: self selectedMethods first methodClass;
		openWithSpec) title: 'Compare methods in ', self selectedMethods first methodClass name
	
	
]

{ #category : #'source code area' }
NautilusUI >> compileAMethodFromCategory: aCategory withSource: aString notifying: aController [
	| category selector class  method |
	category := aCategory.
	class := self selectedClass ifNil: [ ^ Error signal: 'Should not happen!' ].
	(category = self noMethodsString or: [ category = self allLabel ])
		ifTrue: [ category := self asYetUnclassifiedString.
			self categoryWidget resetCategorySelection ].
	selector := self
		compileANewMethodInClass: class
		categorized: category
		from: aString
		notifyng: aController.
	selector ifNil: [ ^ self sourceTextModel hasUnacceptedEdits: true ].
	method := class methodNamed: selector.
	self sourceTextModel clearUserEdits.
	category = Protocol unclassified 
		ifTrue: [ self model classifier classify: method ]. 
	(self selectedMethod notNil and: [ self selectedClass = class and: [ self selectedMethod selector = selector ] ])
		ifFalse: [
			self selectedMethod: method.
			self updateCategoryAndMethodList	].
	
]

{ #category : #'source code area' }
NautilusUI >> compileSource: aText notifying: aController [
	aText ifEmpty: [ ^ self ].
	acceptor accept: aText asString notifying: aController.
]

{ #category : #'package filter' }
NautilusUI >> createPackagePattern: aPatternString [
	^ (aPatternString isNil or: [ aPatternString trimBoth isEmpty ])
		ifTrue: [ nil ]
		ifFalse: [ self ensureSafePackagePattern: aPatternString ]
]

{ #category : #'history behavior' }
NautilusUI >> currentHistoryIndex [

	| index item |
	index := 1 min: self getHistoryList size.
	index = 0 ifTrue: [ ^ 0 ].
	item := (self getHistoryList at: index) key.
	self selectedClass = item selectedClass ifFalse: [ ^ 0 ].
	self selectedMethod ifNil: [ ^ 0 ].
	^ self selectedMethod selector = item selectedMethod
		ifTrue: [ index ]
		ifFalse: [ 0 ]
]

{ #category : #'debugging actions' }
NautilusUI >> debugTest [
	| method |
	method := self selectedMethod.
	method asTestMethod 
		ifNotNil: [:testMethod | 
			self debugTest: testMethod ]
]

{ #category : #'debugging actions' }
NautilusUI >> debugTest: aMethod [
	| breakpoint  |
	breakpoint := Breakpoint new
		node: aMethod ast;
		install.
	[aMethod methodClass run: aMethod selector] ensure: [ breakpoint remove ].
]

{ #category : #'source code area' }
NautilusUI >> defaultClassDescriptor [
	| string |
	string := Slot showSlotClassDefinition
		ifTrue: [ 
			'Object subclass: #NameOfSubclass
	slots: {}
	classVariables: {}
	category: ''' ]
		ifFalse: [ 
			'Object subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' ].
	^ self selectedPackage ifNotNil: [ :package | string , package categoryName , '''' ] ifNil: [ string , '''' ]
]

{ #category : #'menu-packages' }
NautilusUI >> demoteSelectedPackageAsPackageWithTag [

	| packages |
	packages := self selectedPackages.
	
	packages 
		select: [ :each | each name includes: $- ]
		thenDo: [ :each | each demoteToRPackageNamed: (each name copyUpToLast: $-) ]


]

{ #category : #'drag and drop' }
NautilusUI >> dropMethod: aCollectionOfMethods inARow: aRow [

	| newProtocol |
	aRow = 0 ifFalse:  [
		newProtocol := self getCategories at: aRow.
		aCollectionOfMethods do: [:aMethod | self move: aMethod from: aMethod category to: newProtocol isCopy: false ].
		self selectedMethod: nil]
]

{ #category : #'widget category' }
NautilusUI >> elementsMenu: aMenu shifted: b [

	^ aMenu becomeForward: (self methodMenuBuilder menu)
]

{ #category : #'widget category' }
NautilusUI >> enableCategorySingleSelection [

	^ self selectedCategories size <= 1
]

{ #category : #'widget method' }
NautilusUI >> enableMethodDualSelection [

	^ self selectedMethods size = 2
]

{ #category : #'widget method' }
NautilusUI >> enableMethodSingleSelection [

	^ self selectedMethods size <= 1
]

{ #category : #'package filter' }
NautilusUI >> ensureSafePackagePattern: aPattern [
	^ [ aPattern asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | self fixPackagePattern: aPattern ex: ex ]
]

{ #category : #'menus behavior' }
NautilusUI >> fileOutCategories [

	self fileOutCategories: self selectedCategories from: self selectedClass
]

{ #category : #'menus behavior' }
NautilusUI >> fileOutMethods [

	self fileOutMethods: self selectedMethods
]

{ #category : #package }
NautilusUI >> fileOutPackages: aCollection [
	self 
		do: [ :package | package asNautilusSelection item fileOut ]
		on: aCollection
		displaying: 'Filing out packages...'.
]

{ #category : #'menus behavior' }
NautilusUI >> findMethod [
	"Search for a method in the lookup of the selected class"

	| foundMethod class |
	self okToChange
		ifFalse: [ ^ self flashPackage ].
	class := self selectedClass.
	class ifNil: [ ^ self ].
	foundMethod := (SearchFacade messageSearchFor: class)
		chooseFromOwner: self window.
	foundMethod ifNil: [ ^ self ].
	self resetCategoriesListSelection.
	self resetMethodsListSelection.
	self categoryWidget selectProtocol: foundMethod category.
	self model
		package: self selectedPackage
		class: self selectedClass
		protocol: foundMethod protocol
		method: foundMethod.
	self updateCategoryAndMethodList
]

{ #category : #'menus behavior' }
NautilusUI >> findProtocol [
	"Search for a method in the lookup of the selected class"

	| foundProtocol class |
	class := self selectedClass.
	class ifNil: [ ^ self ].
	self okToChange
		ifFalse: [ ^ self flashPackage ].
	foundProtocol := (SearchFacade protocolSearchFor: class)
		chooseFromOwner: self window.
	foundProtocol ifNil: [ ^ self ].
	self resetCategoriesListSelection.
	self categoryWidget selectProtocol: foundProtocol.
	self model
		package: class package
		class: class
		protocol: foundProtocol.
	self updatePackageGroupAndClassList.
	self updateCategoryAndMethodList
]

{ #category : #'package filter' }
NautilusUI >> fixPackagePattern: aPattern ex: anException [
	| fixedPattern |
	fixedPattern := aPattern copyReplaceAll: '*' with: '.*' .

	^  [ fixedPattern asRegexIgnoringCase ]
		on: RegexSyntaxError
		do: [ :ex | '.*' asRegexIgnoringCase ]
]

{ #category : #'widget method' }
NautilusUI >> forceSelectedMethod: aMethod [ 
	
	self model package: self selectedPackage class: self selectedClass protocol: self selectedCategory method: aMethod.
]

{ #category : #'menus behavior' }
NautilusUI >> fullBrowse [
	| ret |
	ret := self model class
		openOnPackage: self selectedPackage
		class: self selectedClass
		category: self selectedCategory
		method: self selectedMethod
		group: self selectedGroup
		sortHierarchically: self sortHierarchically
		showHierarchy: self showHierarchy
		showComment: self showComment
		showInstance: self showInstance
		showCategories: true.
		
	ret packagePatternString: self packagePatternString.
	^ret.
]

{ #category : #'private dispatch' }
NautilusUI >> fullBrowseFromPackage: aPackage [
	self model class
		openOnPackage: aPackage
		class: self selectedClass
		category: self selectedCategory
		method: self selectedMethod
		group: self selectedGroup
		sortHierarchically: self sortHierarchically
		showHierarchy: self showHierarchy
		showComment: self showComment
		showInstance: self showInstance
		showCategories: true
]

{ #category : #'private dispatch' }
NautilusUI >> fullBrowseFromTag: aPackageTag [
	self model class
		openOnPackage: aPackageTag asNautilusSelection
		class: self selectedClass
		category: self selectedCategory
		method: self selectedMethod
		group: self selectedGroup
		sortHierarchically: self sortHierarchically
		showHierarchy: self showHierarchy
		showComment: self showComment
		showInstance: self showInstance
		showCategories: true
]

{ #category : #'widget method' }
NautilusUI >> generateSourceCodeForTestFrom: aMethod [

	^ self buildTestSelectorFor: aMethod.
]

{ #category : #'widget method' }
NautilusUI >> generateTestMethodFor: aMethod [

	"generate a test method for aMethod and return the generated method"
	| class testMethodName|
	testMethodName := aMethod methodClass name, 'Test'.
	class := self model classes detect: [:e | e name = testMethodName] ifNone: [ self createTestForClass: aMethod methodClass ].
	class
		ifNotNil:[ 
			class compile: (self generateSourceCodeForTestFrom: aMethod) classified: 'tests-',aMethod category.
			^ class >> (self buildTestSelectorFor: aMethod) asSymbol ].
	^ nil
]

{ #category : #'menus behavior' }
NautilusUI >> generateTestMethodsAndFocus: aBoolean [
	"generate testing methods for the selected methods in the test class"

	"I assume the test class is named #{MyClassName}Test"

	| focus |
	self selectedMethods ifEmpty: [ ^ self ].
	self selectedMethods reverse
		do: [ :method | focus := self generateTestMethodFor: method ].
	focus ifNil: [ ^ self ].
	(aBoolean and: [ self okToChange ])
		ifTrue:
			[ 
			self selectedPackage: (self parentOfClass: focus methodClass).
			self selectedClass: focus methodClass.
			self selectedCategory: 'tests'.
			methodWidget selectMethod: focus.
			self selectedMethod: focus.
			self updatePackageGroupAndClassList ]
]

{ #category : #'widget category' }
NautilusUI >> getCategories [

	^ categoryWidget getCategories
]

{ #category : #'history behavior' }
NautilusUI >> getHistoryList [
	^ self model historyEntries reversed
]

{ #category : #'widget category' }
NautilusUI >> getMethods [

	^ methodWidget getMethods
]

{ #category : #focus }
NautilusUI >> giveFocusToClass [
	self giveFocusTo: self classWidget
]

{ #category : #focus }
NautilusUI >> giveFocusToMethod [
	self giveFocusTo: methodWidget
]

{ #category : #focus }
NautilusUI >> giveFocusToProtocol [
	self giveFocusTo: categoryWidget
]

{ #category : #accessing }
NautilusUI >> groups [
	^ self model groupsManager groups.
]

{ #category : #'widget method' }
NautilusUI >> highlightCategory: aMethod [

		aMethod 
			ifNotNil: [ | category index |
				category := aMethod protocol.
				category = self selectedCategory
					ifFalse:[ 
						index := self getCategories indexOf: ( category ).
						index > 0 ifTrue: [
							categoryWidget searchedElement: index] ] ]
]

{ #category : #'history behavior' }
NautilusUI >> historyChanged [
	self setWindowTitle.
	self changed: #getHistoryList.
	self changed: #currentHistoryIndex
]

{ #category : #initialization }
NautilusUI >> initialize [
	super initialize.
	categoryWidget := CategoryWidget model: self.
	methodWidget := MethodWidget model: self.
	classWidget := ClassWidget model: self.
	packageWidget := PackageWidget model: self.
]

{ #category : #accessing }
NautilusUI >> interestedInClass: aClass [

	"answer if the browser is showing content related to aClass"
	
	^ (aClass = self selectedClass) or: [ aClass users includes: self selectedClass  ]
]

{ #category : #private }
NautilusUI >> isSelectedPackage: aSelection [ 
         aSelection 
      ifNil:[^ self selectedPackage isNil]
	ifNotNil:[^ aSelection asNautilusSelection = self selectedPackage ]	
]

{ #category : #'widget category' }
NautilusUI >> keyPressedOnElement: anEvent [

		^ self methodWidget keyPressedOnElement: anEvent
]

{ #category : #'events handling' }
NautilusUI >> keyPressedOnList: anEvent shifted: aBoolean [
	(anEvent keyCharacter == self class nextFocusKey
		and: [ (self packageWidget canExpand not or: [ self packageWidget isExpanded ]) ]) 
		ifTrue: [  
			self giveFocusTo: self classWidget.
			^ true ].
	
		^ false

]

{ #category : #'events handling' }
NautilusUI >> keyPressedOnTree: anEvent target: aMorph [ 
	(anEvent keyCharacter == self class nextFocusKey
		and: [ (aMorph canExpand not or: [ aMorph isExpanded ]) ]) 
		ifTrue: [  
			self giveFocusTo: self classWidget.
			^ true ].
	
		^ false

]

{ #category : #private }
NautilusUI >> listClearSelection [
	self packageWidget deselectAll.
]

{ #category : #private }
NautilusUI >> listSelectItem: anObject [ 
	| selection |
	
	selection := anObject asNautilusSelection.
	self packageWidget  
		expandAllFromNode: selection nodePath first;
		selectItems: { selection item }.
	self packageWidget scrollSelectionIntoView.
]

{ #category : #menus }
NautilusUI >> menu: aMenu shifted: aBoolean [ 
	self selectedPackage ifNil: [ ^ aMenu ].

	^ self selectedPackage 
		menu: aMenu 
		shifted: aBoolean
		target: self
]

{ #category : #'system announcements' }
NautilusUI >> metaLinkModified: anAnnouncement [ 
	| method|
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	method := self selectedMethod.
	method ifNil: [ ^self ].
	
	anAnnouncement link methods 
		detect: [ :meth | meth selector = method selector ]
		ifFound: [ 	
				self forceSelectedMethod: method.
				self updateCategoryAndMethodList.
				self highlightCategory: method]
]

{ #category : #'system announcements' }
NautilusUI >> methodAdded: anAnnouncement [
	
	| method |
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	method := anAnnouncement method.
	(self interestedInClass: method methodClass)
		ifTrue: [ 
			self resetMethodsListSelection.
			self removeAllFromMethodsIconsCache: method.
			self updateCategoryAndMethodList.
			(method methodClass isMeta and: [ method methodClass methods size = 1 ])
				ifTrue: [ self changed: #instanceButtonLabel ] ]
]

{ #category : #'system announcements' }
NautilusUI >> methodModified: anAnnouncement [ 
	| collection nonMetaMethodClass |

	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	collection := self classWidget getClassesList.
	nonMetaMethodClass:=anAnnouncement methodClass.
	self showInstance ifFalse:[nonMetaMethodClass:=nonMetaMethodClass theNonMetaClass].
	((collection includes: nonMetaMethodClass) or: [ anAnnouncement methodClass users includes: self selectedClass  ])
		ifTrue: [
			| method selectedMethod oldMethod |
			method := anAnnouncement newMethod.
			selectedMethod := self selectedMethod.
			self removeFromMethodsIconsCache: method.
			self updateClassView.
			oldMethod := anAnnouncement oldMethod.
			(selectedMethod = oldMethod)
				ifTrue: [
					self sourceTextModel hasUnacceptedEdits
						ifTrue: [
							self forceSelectedMethod: method.
							self updateCategoryAndMethodList.
							self highlightCategory: method]
						ifFalse: [
							self selectedMethod: method.
							self updateCategoryAndMethodList ]]
				ifFalse: [self updateCategoryAndMethodList  ] ]
]

{ #category : #'system announcements' }
NautilusUI >> methodRecategorized: anAnnouncement [
	
	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [ ^ self ].
	
	(self interestedInClass: anAnnouncement methodClass)
		ifTrue: [self updateCategoryAndMethodList ]
]

{ #category : #'system announcements' }
NautilusUI >> methodRemoved: anAnnouncement [
	| method |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	method := anAnnouncement method.
	(self interestedInClass: method methodClass)
		ifTrue: [ 
			self sourceTextModel hasUnacceptedEdits
				ifFalse: [self selectNextMethod].
			self removeAllFromMethodsIconsCache: method.
			method isTestMethod
				ifTrue:
					[self updateClassView ].	"Should update class icon"
			self resetMethodsList.
			methodWidget updateMethodList.
			(method methodClass isMeta and: [ method methodClass hasMethods not ])
				ifTrue: [ self changed: #instanceButtonLabel ] ]
]

{ #category : #private }
NautilusUI >> methodSortBlock [
	^ [ :a :b |
		"If one is binary and the other isn't, sort the binary first.
		Otherwise, sort them by Character values."
		a selector isBinary = b selector isBinary
			ifTrue: [ a selector <= b selector ]
			ifFalse: [ a selector isBinary ] ]
]

{ #category : #'widget method' }
NautilusUI >> methodWidget [

	^ methodWidget
]

{ #category : #'widget method' }
NautilusUI >> methodsForCategories: aCollection [

	^ aCollection flatCollect: [:e | self methodsForCategory: e ]
]

{ #category : #'widget method' }
NautilusUI >> methodsForCategory: aCategory [
	| methods class env |
	class := self selectedClass.
	class ifNil: [ ^ #() ].
	env := self browsedEnvironment.
	methods := env isSystem
		ifTrue: [ self methodsInSystemEnvironmentForCategory: aCategory in: class ]
		ifFalse: [ self methodsInARestrictedEnvironment: env forCategory: aCategory in: class ].
	^ methods sort: self methodSortBlock.
]

{ #category : #'widget method' }
NautilusUI >> methodsIconsCache [
	
	^ self methodWidget methodsIconsCache
]

{ #category : #'widget method' }
NautilusUI >> methodsInARestrictedEnvironment: env forCategory: aCategory  in: class [
	| mthds selectors |
	
	aCategory = self allLabel
		ifTrue: [ ^ class methods asOrderedCollection select: [:e | env includesSelector: e selector in: class ]].
	selectors := env selectorsFor: aCategory in: class.
	mthds := OrderedCollection new.
	selectors do: [:s | mthds add: (class compiledMethodAt: s) ].
	^ mthds
]

{ #category : #'widget method' }
NautilusUI >> methodsInSystemEnvironmentForCategory: aCategory  in: class [
	
	aCategory = self allLabel
		ifTrue: [ ^ class methods asOrderedCollection ].
	
	^ (class methodsInProtocol: aCategory) asOrderedCollection
]

{ #category : #'widget method' }
NautilusUI >> methodsLabel [

	^ methodWidget methodsLabel
]

{ #category : #'widget method' }
NautilusUI >> methodsSelection [

	^ methodWidget methodsSelection
]

{ #category : #'drag and drop' }
NautilusUI >> move: item from: oldCategory to: newCategory isCopy: isCopy [

	| category |
	category := newCategory.
	category = self allLabel
		ifTrue: [ category := self asYetUnclassifiedString ].
	isCopy
		ifFalse: [
			item methodClass organization
				classify: item selector
				under: category 
				suppressIfDefault: true.
			self selectedMethod: item ] 
]

{ #category : #'menus behavior' }
NautilusUI >> moveMethodToPackage [
	"Search for a package from a pattern or from the recent list"	
	
	| foundPackage  |	
	self okToChange ifFalse: [^ self flashPackage ].
	foundPackage := SearchFacade rPackageSearch chooseFromOwner: self window.
	foundPackage ifNil: [ ^self ].
	self selectedMethods do: [ :each | self moveMethod: each inPackage: foundPackage ].
	self updateCategoryAndMethodList.
		

]

{ #category : #'history behavior' }
NautilusUI >> next [
	self model hasNext 
		ifFalse: [ ^ self ].
	"show the next item in history"
	self model next
]

{ #category : #'widget method' }
NautilusUI >> notifyTitle: title contents: contents [

	 GrowlMorph 
		openWithLabel: title
		contents: contents
]

{ #category : #'widget method' }
NautilusUI >> notifyTitle: title contents: contents color: aColor [
	^self class notifyTitle: title contents: contents color: aColor
]

{ #category : #open }
NautilusUI >> open [
	| focus |
	super open.
	window takeKeyboardFocus.
	focus := self packageWidget.
	self selectedClass
		ifNotNil: [ acceptor := ClassOrMethodDefinitionAcceptor model: self.
			focus := self classWidget ].
	self selectedCategory
		ifNotNil:
			[ :cat | 
			categoryWidget selectProtocol: cat.
			self resetMethodsList.
			acceptor := MethodDefinitionAcceptor model: self.
			focus := categoryWidget ].
	self selectedMethod
		ifNotNil: [ :meth | 
			methodWidget selectMethod: meth.
			acceptor := MethodDefinitionAcceptor model: self.
			focus := methodWidget ].
	self selectedClass ifNotNil: [ self updateCategoryAndMethodList: self selectedMethod ].
	self giveFocusTo: focus.
]

{ #category : #'package filter' }
NautilusUI >> package: aRPackage matchPackagePattern: aPattern [ 
	^ aPattern
		ifNil: [ true ]
		ifNotNil: [ :pattern | (pattern search: aRPackage name) or:[
				aRPackage classTags anySatisfy: [:tag | pattern search: tag name]]]
]

{ #category : #'menu pragmas' }
NautilusUI >> packageFixPragma [
	^ 'nautilusGlobalPackageTreeFixMenu'
]

{ #category : #'package filter' }
NautilusUI >> packageMatchPattern: aRPackage [ 
	^ self package: aRPackage matchPackagePattern: self packagePattern
]

{ #category : #'package filter' }
NautilusUI >> packagePattern [
	^ packagePattern
]

{ #category : #'package filter' }
NautilusUI >> packagePatternString [
	
	^ searchWidget searchString
]

{ #category : #'package filter' }
NautilusUI >> packagePatternString: aStringOrNil [
	
	aStringOrNil ifNil: [ ^ self ].
	searchWidget content: aStringOrNil
]

{ #category : #'package filter' }
NautilusUI >> packageSearchUpdate: aPatternString [
	packagePatternString = aPatternString ifTrue: [ ^ self ].
	packagePatternString := aPatternString.
	packagePattern := self createPackagePattern: aPatternString.
	SharedPackageFilterPatternsHolder instance lastValue: aPatternString.
	self updatePackages
]

{ #category : #'widget package' }
NautilusUI >> packageWidget [

	^ packageWidget
]

{ #category : #accessing }
NautilusUI >> packages [ 
	^ self model packages
]

{ #category : #private }
NautilusUI >> parentOfClass: aClass [
	^ self model parentOfClass: aClass
]

{ #category : #'history behavior' }
NautilusUI >> previous [
	self model hasPrevious 
		ifFalse: [ ^ self ].
	"show the previous item in the history"
	self model previous
]

{ #category : #'menu-packages' }
NautilusUI >> promoteSelectedPackageTagAsPackage [
	self selectedPackage item promoteAsRPackage
]

{ #category : #refactorings }
NautilusUI >> refactor [

	^ NautilusRefactoring model: self
]

{ #category : #refactorings }
NautilusUI >> refactorCode: aText [
	acceptor := CodeRefactoringAcceptor model: self.
	self sourceCode: aText
]

{ #category : #'private dispatch' }
NautilusUI >> registerSelectionGroup: aSelection [
	self model selectedGroup: aSelection item.
	
	self flag: #todo. "I need to refactor to remove the need of groups"
	"I select also group-as-a-package, because I will be filtering with package selection, 
	 not with group selection (which actually does not has much sense now)"
	self model 	
		package: aSelection
		class: self selectedClass.
	
	self packageWidget selectGroup: aSelection item.

]

{ #category : #'private dispatch' }
NautilusUI >> registerSelectionPackage: aSelection [

	| selectedClass |
	
	selectedClass := self selectedClass.

	selectedClass
		ifNil: [ self model package: aSelection ]
		ifNotNil: [ self model package: aSelection class: selectedClass ].
		
	self packageWidget selectPackage: aSelection item.
]

{ #category : #'widget method' }
NautilusUI >> removeAllFromMethodsIconsCache: aMethod [

	methodWidget removeAllFromMethodsIconsCache: aMethod
]

{ #category : #'menus behavior' }
NautilusUI >> removeCategories [

	self selectedClass 
		ifNotNil: [:class |
			self selectedCategories
				ifNotEmpty: [:items || removeACategory scroll |
					scroll := categoryWidget vScrollValue.
					removeACategory := false.
					items do: [:item || needToUpdate |
						needToUpdate := self removeCategory: item inClass: class.
						(needToUpdate and: [ removeACategory not ]) ifTrue: [ removeACategory := true].
						needToUpdate ifTrue: [ self updateCategoryAndMethodList ]].

					removeACategory ifTrue: [ self selectedCategory: nil ].
					self updateCategoryAndMethodList.
					categoryWidget vScrollValue: scroll. ]]
]

{ #category : #'private dispatch' }
NautilusUI >> removeClassCategory: packageTag [ 
	self basicRemoveClasses: packageTag classes.
	packageTag asNautilusSelection item removeFromPackage.
]

{ #category : #refactorings }
NautilusUI >> removeClassWithRefactoringEngine: aCollection [

	^ self refactor removeClasses: aCollection 
]

{ #category : #'menus behavior' }
NautilusUI >> removeGroups [
	self okToChange ifFalse: [ ^ self ].
	
	super removeGroups.
	self updatePackageView.
]

{ #category : #'menus behavior' }
NautilusUI >> removeMethod: aMethod inClass: aClass [
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so, remove it. "
	
	| result |
	self okToChange ifFalse: [^ false].
	result := super removeMethod: aMethod inClass: aClass.
	result ifTrue: [ self updateCategoryAndMethodList ].
	^result
]

{ #category : #'menus behavior' }
NautilusUI >> removeMethods [

	self selectedClass 
		ifNotNil: [:class |
			self selectedMethods
				ifNotEmpty: [:items || scroll needToUpdate |
					scroll := methodWidget vScrollValue.
					needToUpdate := self removeMethods: items inClass: class.
					
					needToUpdate ifTrue: [ 
						self resetMethodsListSelection.
						methodWidget updateMethodList ].
					
					(class methods includes: self selectedMethod)
						ifFalse: [ self selectedMethod: nil ].
						
					self resetMethodsListSelection.
					methodWidget
						updateMethodList;
						vScrollValue: scroll. ]]
]

{ #category : #'menus behavior' }
NautilusUI >> removeNonLocalSelector: aSymbol [
	| traits isAlias |
	traits := self selectedClassOrMetaClass traitsProvidingSelector: aSymbol.
	isAlias := self selectedClassOrMetaClass isLocalAliasSelector: aSymbol.
	isAlias
		ifTrue: [
			[traits size = 1] assert.
			self selectedClassOrMetaClass removeAlias: aSymbol of: traits first]
		ifFalse: [
			traits do: [:each |
				self selectedClassOrMetaClass addExclusionOf: aSymbol to: each ]]
	
]

{ #category : #'private dispatch' }
NautilusUI >> removePackageCategory: packageSelection [

	packageSelection item removeFromSystem.
]

{ #category : #'menus behavior' }
NautilusUI >> removePackages [
	| names packages |
	
	packages := self selectedPackages. 
	packages ifEmpty: [ ^ self ].
	
	names := (packages 
		collect: [:package | package name ])
		joinUsing: String cr.

	(UIManager default 
		confirm: ('Are you sure you want to delete the selection(s) named ', String cr, names, ' and their classes ?'))
		ifTrue: [ packages do: [ :each | each asNautilusSelection removeTarget: self ].
			self updatePackageView ]
]

{ #category : #'menus behavior' }
NautilusUI >> renameCategory [
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"

	| category newName vScroll protocol |
	
	self okToChange ifFalse: [^ self].
	
	protocol  := self selectedProtocol. 
	protocol ifNil: [ ^ self ].
	
	protocol canBeRenamed 
		ifFalse: [ ^ self inform: ('You can not rename "{1}" protocol.' format: { protocol name })  ].
	category := self selectedCategory.
	vScroll := categoryWidget vScrollValue.
	newName := self renameCategory: category from: self selectedClass.
	newName ifNil: [ ^ self ].
	categoryWidget deselectProtocol: category.
	self selectedCategory: newName.
	categoryWidget selectProtocol: newName.
	self updateCategoryAndMethodList.
	categoryWidget vScrollValue: vScroll
]

{ #category : #'private dispatch' }
NautilusUI >> renameClassCategory: packageTag [
	| newName oldName |

	oldName := packageTag name.
	newName := UIManager default 
		request: ('New name of category {1}' format: { packageTag name }) 
		initialAnswer: oldName
		title: 'Rename a category'.
	newName = oldName ifTrue: [ ^ self ].
	
	(self isValidPackageName: newName)
		ifFalse: [ ^ self alertInvalidPackageName:newName ].		
	(self category:newName existsIn:packageTag package)
		ifTrue:[ ^  self alertCategoryName:newName exitstsAlreadyIn:packageTag package].
	packageTag renameTo: newName.


]

{ #category : #refactorings }
NautilusUI >> renameClassWithRefactoringEngine: aClass [

	self refactor renameClass: aClass 
]

{ #category : #private }
NautilusUI >> renamePackage: aSelection [
	aSelection asNautilusSelection renameTarget: self. 

]

{ #category : #'private dispatch' }
NautilusUI >> renamePackageCategory: package [
	self basicRenamePackage: package
]

{ #category : #selection }
NautilusUI >> resetAllListSelections [

	self resetPackageGroupsListSelection.
	self resetClassesListSelection.
	self resetCategoriesListSelection.
	self resetMethodsListSelection.
]

{ #category : #selection }
NautilusUI >> resetCategoriesList [
	self categoryWidget resetCategoryList
]

{ #category : #selection }
NautilusUI >> resetCategoriesListSelection [

	categoryWidget resetCategorySelection
]

{ #category : #selection }
NautilusUI >> resetClassesList [
	self classWidget resetClassList
]

{ #category : #selection }
NautilusUI >> resetClassesListSelection [

	classWidget resetClassSelection 
]

{ #category : #selection }
NautilusUI >> resetMethodsList [
	self methodWidget resetMethodList
]

{ #category : #selection }
NautilusUI >> resetMethodsListSelection [

	methodWidget resetMethodSelection
]

{ #category : #selection }
NautilusUI >> resetPackageGroupsListSelection [
	self packageWidget resetPackagesSelection.
	self packageWidget resetGroupsSelection.
]

{ #category : #'menus behavior' }
NautilusUI >> restrictedBrowsePackages: selections [
	^ super restrictedBrowsePackages: (selections collect: [ :each | each asNautilusSelection item ])
]

{ #category : #refactorings }
NautilusUI >> rewriteCode: aText [

	acceptor := CodeRewritingAcceptor model: self.
	self sourceCode: aText
]

{ #category : #private }
NautilusUI >> runPackagesTestsNotifying: aBoolean [
	| packages classes label |

	(packages := self selectedPackages) ifNil: [ ^ self ].
	label := 
	packages size > 1
		ifTrue: [ 'Packages: ' ]
		ifFalse: [ 'Package: ' ].
		
	classes := packages flatCollect: [ :package | package asNautilusSelection definedClasses ].
	classes := classes select: [ :class | class isTestCase ].
	
	self runClassTests: classes notifying: false.

	aBoolean ifTrue: [ self notifyTestResultFrom: packages classes: classes labeled: label ].
]

{ #category : #'menus behavior' }
NautilusUI >> runTestForAMethod: aMethod notifying: aBoolean [
	^self class runTestForAMethod: aMethod notifying: aBoolean
]

{ #category : #refactorings }
NautilusUI >> searchCode: aText [
	acceptor := CodeSearchingAcceptor model: self.
	self setStylerClass: nil.
	self pendingText: aText
]

{ #category : #selection }
NautilusUI >> selectNextMethod [
	methodWidget selectNextMethod
]

{ #category : #private }
NautilusUI >> selectPackageFor: aClass [
	"I'm called to properly select a class pache/tag.
	 Since we are showing root tags as packages when their are the only ones, we need to determine 
	 if we are going to select the package or the tag..."

	| package tag |
	package := aClass package.
	tag := package classTagForClass: aClass.
	self
		selectedPackage:
			((tag isRoot and: [ package classTags size = 1 ])
				ifTrue: [ tag package ]
				ifFalse: [ tag ])
]

{ #category : #'widget category' }
NautilusUI >> selectedCategories [

	^ categoryWidget selectedCategories
]

{ #category : #'widget category' }
NautilusUI >> selectedCategory [

	^ (self model 
			ifNil: [ ^ nil ]) selectedCategory
]

{ #category : #'widget category' }
NautilusUI >> selectedCategory: protocol [
	self okToChange
		ifTrue: [ 
			self resetMethodsList.
			self model
				package: self selectedPackage
				class: self selectedClass
				protocol: protocol.
			self resetMethodsListSelection.
			protocol
				ifNil: [ acceptor := ClassOrMethodDefinitionAcceptor model: self ]
				ifNotNil: [ 
					acceptor := MethodDefinitionAcceptor model: self.
					self giveFocusTo: categoryWidget ].
			categoryWidget changed: #selectedCategoryIndex.
			^ true ].
	^ false
]

{ #category : #accessing }
NautilusUI >> selectedCategoryInternally: anObject [
	self resetMethodsListSelection.
	anObject ifNotNil: [ self giveFocusTo: categoryWidget ]
]

{ #category : #private }
NautilusUI >> selectedClass: aClass belongsToPackageSelection: aPackage [
	"test whether the class belongs to a selected package. But if
we are in hierarchy view, the membership is more difficult to compute. Now, I just
assmume, if hierarchy view is enabled, selecting this class is what we want"

	^ (aPackage includesClass: aClass) or: [ self showHierarchy ]
]

{ #category : #private }
NautilusUI >> selectedClass: aClass withSelection: aPackage [
	self model
		package: aPackage
		class: aClass.
	self resetPackageGroupsListSelection.
	self packageWidget selectPackage: aPackage.
	self updatePackageGroupAndClassList
]

{ #category : #accessing }
NautilusUI >> selectedClassWithoutChangingSelection: aClass [
	"protocol: accessing"

	self resetCategoriesList.
	self okToChangeSourceAndComment
		ifFalse: [ ^ self ].
	aClass
		ifNil: [ self unselectClass ]
		ifNotNil: [ 
			acceptor := ClassOrMethodDefinitionAcceptor model: self.
			self classWidget selectClass: aClass.
			((self selectedPackageIncludes: aClass theNonMetaClass) or: [ model showHierarchy ])
				ifTrue: [ 
					self model
						package: self selectedPackage
						class: aClass ]
				ifFalse: [ self selectedClass: aClass withSelection: (self parentOfClass: aClass) ].
			self updateOnClassSelection ]
]

{ #category : #accessing }
NautilusUI >> selectedClassWithoutChangingSelectionInternally: aClass [
	| package packageChanged |
	package := self selectedPackage.
	self showHierarchy
		ifTrue: [ aClass ifNotNil: [ package := aClass package ] ].
	aClass ifNil: [ self resetClassesListSelection ].
	aClass ifNotNil: [ self giveFocusTo: self classWidget ].
	packageChanged := package ~= self selectedPackage.
	self classWidget selectClass: aClass.
	self model selectedClass: aClass.
	self setWindowTitle.
	packageChanged
		ifTrue:
			[ 
			self resetPackageGroupsListSelection.
			self packageWidget selectPackage: package.
			self updatePackageGroupAndClassList ].
	self resetCategoriesListSelection.
	self resetMethodsListSelection.
	self updateCategoryAndMethodList.
	self changed: #isAClassSelected
]

{ #category : #accessing }
NautilusUI >> selectedFromPackageGroupView: aNode [
	| selections |

	selections := aNode selectedNodes collect: [ :each |each asNautilusSelection].
	"leave if selection contains no node that isn't already selected"
	(selections allSatisfy:[:each | self isSelectedPackage:each]) ifTrue:[^ self].
	
	self clearPackageSelections.

	selections do:[: each |  self selectedPackageWithoutChangingSelection: each].
	
	self changed: #currentHistoryIndex.

]

{ #category : #accessing }
NautilusUI >> selectedGroup: aGroup [
	" Force package selection, not used by the lists "

	self okToChange ifFalse: [ ^ self ].
			
	acceptor := ClassDefinitionAcceptor model: self.
			
	self resetPackageGroupsListSelection.
	self packageWidget selectGroup: aGroup.
		
	aGroup 
		ifNotNil: [ 
			self basicSelectPackage: aGroup ]
		ifNil: [
			self selectedPackageWithoutChangingSelection: nil. 
			self listClearSelection ].	
		

	
]

{ #category : #'widget method' }
NautilusUI >> selectedMethod [

	^ self model selectedMethod 
]

{ #category : #'widget method' }
NautilusUI >> selectedMethod: aMethod [ 

	self okToChange
		ifTrue: [
			self forceSelectedMethod: aMethod.
			self setWindowTitle.
			self highlightCategory: aMethod.
			aMethod 
				ifNil: [ 
					acceptor := self selectedCategory 
						ifNil: [ ClassOrMethodDefinitionAcceptor model: self ]
						ifNotNil:[ MethodDefinitionAcceptor model: self ]]
				ifNotNil: [ 
					acceptor := MethodDefinitionAcceptor model: self.
					self giveFocusTo: methodWidget ].
			^ true].
	^false 
]

{ #category : #'widget method' }
NautilusUI >> selectedMethods [

	^ methodWidget selectedMethods
]

{ #category : #accessing }
NautilusUI >> selectedPackage: aPackage [
	" Force package selection, not used by the lists "
	
	self okToChange ifFalse: [ ^ self ].

	acceptor := ClassDefinitionAcceptor model: self.
			
	self resetPackageGroupsListSelection.
	self packageWidget selectPackage: aPackage.
		
	aPackage 
		ifNotNil: [ 
			self basicSelectPackage: aPackage ]
		ifNil: [
			self selectedPackageWithoutChangingSelection: nil.
			self listClearSelection ].	
		
	
]

{ #category : #'widget package' }
NautilusUI >> selectedPackageIncludes: aClass [
^ (self selectedPackage notNil) and:[
	 (self selectedPackage classes includes: aClass ) or:[
		self selectedPackage classes includes: aClass class ].
	]
]

{ #category : #accessing }
NautilusUI >> selectedPackageWithoutChangingSelection: aSelection [
	| class |
	self okToChange
		ifFalse: [ ^ self ].
	class := self selectedClass.
	(class notNil and: [ aSelection isNil or: [ (self selectedClass: class belongsToPackageSelection: aSelection) not ] ])
		ifTrue: [ self selectedClass: nil ].
	aSelection ifNotNil: [ aSelection registerSelectionTarget: self ].
	self changed: #browseScopedButtonEnabled.
	self updateClassView.
	self updateCategoryAndMethodList.
	self commentTextModel setInitialText: self getComments.
]

{ #category : #accessing }
NautilusUI >> selectedProtocol [
	^ self selectedCategory 
		ifNotNil: [ :protocolName | self selectedClass organization protocolNamed: protocolName ]
]

{ #category : #'history behavior' }
NautilusUI >> setHistory: anIndex [
	| entry |
	anIndex == self currentHistoryIndex ifTrue: [ ^ self ].
	entry := self getHistoryList at: anIndex ifAbsent: [ nil ].
	entry ifNil: [ ^ self ].
	self okToChange ifTrue:[self model adopt: entry.].
	self changed: #currentHistoryIndex.
]

{ #category : #accessing }
NautilusUI >> showHierarchy: aBoolean [
	|selectedClass|
	
	(selectedClass := self selectedClass) ifNil: [ ^ self ]. "Since the double clic comes after a simple clic and simple clic deselects selected item, we are forced to have this guard. A alternative would be to change the simple clic but the side effects are too big"
	self resetClassesListSelection.
	self resetClassesList.
	self classWidget showHierarchy: aBoolean forClass: selectedClass theNonMetaClass.

	"If showPackages false(Hierarchy)-->true(Flat) then selected package needs to synch to that of selected class"
	self packageWidget enabled: aBoolean not.
	(aBoolean not and: [ self model showHierarchy and: [ self selectedClass notNil ]])
		ifTrue: [ 
			(self selectedPackage includesClass: self selectedClass class)
				ifFalse: [ self selectPackageFor: self selectedClass ] ]. 
	"We check if we had a selectedMethod before toggling hierarchy button"
	self model showHierarchy: aBoolean.
	
	self updateClassView.
	self classWidget selectClass: self selectedClass.
	self updateClassView.	
	self changed: #hierarchyButtonState
]

{ #category : #'widget method' }
NautilusUI >> signatureFor: aSelector [

	^ (aSelector includes: $:)
		ifTrue: [ | col |
			col := aSelector substrings: ':'.
			String streamContents: [:s |
					col doWithIndex: [:e :i |
						i == 1 
							ifTrue: [ s << e << $: << ' anObject ' ]
							ifFalse: [ s << e << $: << ' anObject'<< (i-1) printString << ' ' ]]]]
		ifFalse: [ aSelector ]
]

{ #category : #'menus behavior' }
NautilusUI >> toggleBreakConditionOnEntryIn: aMethod [
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint ifFalse: [ 
		(UIManager default request: 'Break on what condition?')	
		ifNotNil: [ :condition | Breakpoint new 
			node: aMethod ast;
			condition: (aMethod class compiler evaluate: condition);
			install]
		]
]

{ #category : #'menus behavior' }
NautilusUI >> toggleBreakOnceOnEntryIn: aMethod [
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint ifFalse: [ Breakpoint new node: aMethod ast; once; install]
]

{ #category : #'menus behavior' }
NautilusUI >> toggleBreakpoint [
	| savedSelectedMethodIndex savedVscroll |
	self selectedMethod ifNil: [ ^ self ].

	savedSelectedMethodIndex := methodWidget selectedMethodIndex.
	savedVscroll := methodWidget vScrollValue.
	self class toggleBreakpointIn: self selectedMethod.
	self resetMethodsList.
	
	methodWidget invalidateCacheEntryFor: self selectedMethod.
	methodWidget selectedMethodIndex: savedSelectedMethodIndex.
	savedVscroll := methodWidget vScrollValue: savedVscroll
]

{ #category : #'menus behavior' }
NautilusUI >> toggleBreakpointCondition [
	| savedSelectedMethodIndex savedVscroll |
	self selectedMethod ifNil: [ ^ self ].
	savedSelectedMethodIndex := methodWidget selectedMethodIndex.
	savedVscroll := methodWidget vScrollValue.
	self toggleBreakConditionOnEntryIn: self selectedMethod.
	self resetMethodsList.
	
	methodWidget invalidateCacheEntryFor: self selectedMethod.
	methodWidget selectedMethodIndex: savedSelectedMethodIndex.
	savedVscroll := methodWidget vScrollValue: savedVscroll
]

{ #category : #'menus behavior' }
NautilusUI >> toggleBreakpointOnce [
	| savedSelectedMethodIndex savedVscroll |
	self selectedMethod ifNil: [ ^ self ].
	savedSelectedMethodIndex := methodWidget selectedMethodIndex.
	savedVscroll := methodWidget vScrollValue.
	self toggleBreakOnceOnEntryIn: self selectedMethod.
	self resetMethodsList.
	
	methodWidget invalidateCacheEntryFor: self selectedMethod.
	methodWidget selectedMethodIndex: savedSelectedMethodIndex.
	savedVscroll := methodWidget vScrollValue: savedVscroll
]

{ #category : #private }
NautilusUI >> unselectClass [
	acceptor := ClassDefinitionAcceptor model: self.
	self resetClassesListSelection.
	self model
		package: self selectedPackage.
	self updateOnClassSelection
]

{ #category : #updating }
NautilusUI >> updateCategoryAndMethodList [
	self resetCategoriesList.
	categoryWidget updateCategoryList.
	self resetMethodsList.
	methodWidget updateMethodList
]

{ #category : #updating }
NautilusUI >> updateCategoryAndMethodList: aMethodOrNil [
	self resetCategoriesList.
	categoryWidget updateCategoryList.
	self resetMethodsList.
	methodWidget updateMethodList: aMethodOrNil
]

{ #category : #private }
NautilusUI >> updateGroupView [ 
	self updatePackageView
]

{ #category : #private }
NautilusUI >> updateOnClassSelection [
	self changed: #hierarchyButtonEnabled.
	self changed: #browseVariablesButtonEnabled.
	self setWindowTitle.
	self resetCategoriesListSelection.
	self resetMethodsListSelection.
	self updateCategoryAndMethodList.
	self changed: #isAClassSelected.
	self changed: #instanceButtonLabel.
	self commentTextModel setInitialText: self getComments.

]

{ #category : #private }
NautilusUI >> updatePackageGroupAndClassList [
	| selectedPackage selectedClass |
	
	
	selectedPackage := self selectedPackage.
	selectedClass := self selectedClass.
	super updatePackageGroupAndClassList.
	"I need to restore selection (otherwise, nothing goes to the right position)"
	self packageWidget updateList.
	self selectedPackage: selectedPackage.
	(selectedPackage isNil or: [ selectedClass isNil ])
		ifTrue: [ ^ self ].
	"only reselect the former class"
	"if it is still in this package"
	(self selectedClass: selectedClass belongsToPackageSelection: selectedPackage)
		ifTrue: [ self selectedClass: selectedClass ]
]

{ #category : #private }
NautilusUI >> updatePackageView [
	self packageWidget updatePackageView
]

{ #category : #'package filter' }
NautilusUI >> updatePackages [
	| package |
	package := self selectedPackage.
	package 
		ifNotNil: [ 
			self packageWidget 
					updateContentsWithPreviouslyExpanded: package item asNautilusItemPath;
					scrollSelectionIntoView ]
		ifNil: [ self packageWidget updateContentsWithPreviouslyExpanded: #() ]
]

{ #category : #'package filter' }
NautilusUI >> useLastPackagePatternString [
	self packagePatternString: SharedPackageFilterPatternsHolder instance lastValue
]

{ #category : #'package filter' }
NautilusUI >> useLastPackagePatternStringForClass: aClass [
	| patternString |
	patternString := SharedPackageFilterPatternsHolder instance lastValue.
	patternString ifNil: [ ^ self ].
	(self class: aClass matchPackagePatternString: patternString)
		ifFalse: [ patternString := patternString, '|', aClass package name ].
	self packagePatternString: patternString.
]

{ #category : #'history behavior' }
NautilusUI >> wrapHistory: anEntry [
	^ StringMorph contents: anEntry key asHistoryString
]

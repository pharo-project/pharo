"
<help>

! The User interface
A SettingBrowser allows the browsing as well as the editing of setting values.
For each setting, a label and an input widget allowing the change of the setting value are shown.

!! Browsing standards settings
In order to open a setting browser one can also use the expression below:
--------------------
SettingBrowser open
--------------------
It opens a SettingBrowser for all settings declared with the standard 'systemsettings' pragma keyword. 
To see how these settings are declared in the image, you can browse all senders of #systemsettings:
--------------
SystemNavigation new  browseAllSendersOf: #systemsettings
--------------

!! Browsing application specific settings
For specific applications, other pragma keywords can be used. These keywords can be passed as follow:
--------------------
(SettingBrowser forKeywords: #('blob')) open.
--------------------
Then, only settings which are declared with the keyword 'blob' are viewed. 
Here is an example of such a setting declared in the class side of a class BlobSettings 
--------------------
BlobSettings class>>blobSettingOn: aBuilder
	<blob>
	(aBuilder group: #blobEditing) 
		label: 'Editing' translated;
		parent: #blobBrowsing; 
		description: 'All settings concerned with blob editing' translated;
		with: [	
			(aBuilder setting: #color)	...
--------------------
The expression 'SettingBrowser open' is then equivalent to the '(SettingBrowser forKeywords: #('systemsettings')) open' expression.

!! Filtering
The SettingBrowser has a filtering functionality to limitate the number of settings. You can enter a token in the input field of the toolbar in order to show only settings that are matching the token.
Two filtering modes are allowed:  with a substring or with a regular expression.
- with a substring: only settings which name, description, pragma class or method selector includes this substring are viewed; 
- with a regular expression: the same as with a substring except that the input is used as a regular expression. This filtering is applied if the Regexp checkbox is checked.

! How to declare a setting
A setting is declared with a method class side. This kind of method takes a builder as argument and a standard setting is tagged with the <systemsettings> pragma. The builder argument serves as a facade for the declaration.

!!A simple boolean setting example

Let's start with a setting declaration example:
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the ""find"" command in text will always make its searches in a case-sensitive fashion' translated;
		setSelector: #caseSensitiveFinds:;
		getSelector: #caseSensitiveFinds;
		target: TextEditor;
		parent: #codeEditing.

For this setting to be declared, we make the asumption that we have TextEditor class>>caseSensitiveFinds and TextEditor class>>caseSensitiveFinds: methods in order to change the preference value. 
To declare a setting, just send #setting: to the builder with its identifier, a Symbol passed as argument. It creates a setting node. Then you can set the label, the description with #label: and #description sent to the newly created setting node. You also have to set the selectors for setting and getting the preference value as well as the target to which these accessors are sent  (often a class). This is done by sending respectively, #setSelector:, #getSelector: and #target: to the setting node.
Because all settings are organized in trees we need a way to indicate what is the position of the setting node in the overall setting trees list. In fact it can be done two ways. The first way is to use the #parent: message (A second possibility is to declare a subtree in one method, it is explained later in this documentation).The #parent: message is send for non root settings. #parent takes the identifier of the parent setting as argument.

You may notice that in this example,  if we don't take into account the $: at the end of the setting accessor, the getting and the setting accessors are the same. This is often the case. You can simply set the setter and the getter by sending the #selector: message to the setting node. Thus the declaration is simplified as follow:
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the ""find"" command in text will always make its searches in a case-sensitive fashion' translated;
		selector: #caseSensitiveFinds;
		target: TextEditor;
		parent: #codeEditing.

You may also notice that the identifier of the setting is then the same as the selector. In this case, you can omit to set the selector because by default, the identifier is used as the selector for getting the preference value and the identifier concatenated with a $: at the end is used as the setting selector. Thus the declaration is simplified again as follow:
CodeHolderSystemSettings class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the ""find"" command in text will always make its searches in a case-sensitive fashion' translated;
		target: TextEditor;
		parent: #codeEditing.

Finally, if you decide to declare a setting directly in the class which implements the selector, the target information can be also omitted because the class in which the setting is declared is used by default. Thus the simplest declaration can be:
TextEditor class>>caseSensitiveFindsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #caseSensitiveFinds) 
		label: 'Case sensitive search' translated;
		description: 'If true, then the ""find"" command in text will always make its searches in a case-sensitive fashion' translated;
		parent: #codeEditing.

!!Declaring a subtree in one single method
Directly declaring a sub-tree of settings in one method is also possible. Then, typically, a root group is declared for the application settings and the children settings themselves are also declared within the same method as in the example below in which #formatCommentWithStatements and #indentString are directly declared as children of the #configurableFormatter setting:

RBConfigurableFormatter class>>settingsOn: aBuilder
	<systemsettings>	
	(aBuilder group: #configurableFormatter)
		target: self;
		parent: #refactoring;
		label: 'Configurable Formatter' translated;
		description: 'Settings related to the formatter' translated;
		with: [
			(aBuilder setting: #formatCommentWithStatements)
				label: 'Format comment with statements' translated.
			(aBuilder setting: #indentString)
				label: 'Indent string' translated]

!!Optional setting
Any setting can have children. In the case where a boolean setting is used as a parent, then, its children are chown only if the parent preference value is true.

!!Range setting
You send the #range: message to the builder instead of the #setting: message. In addition, you send the #range: message to the setting with an interval as argument in order to indicate the valid range.
screenMarginSettingOn: aBuilder
	<systemsettings>
	(aBuilder range: #fullscreenMargin)
		target: SystemWindow;
		parent: #windows;
		label: 'Fullscreen margin' translated;
		description: 'Specify the amount of space that is let around a windows when it''s opened fullscreen' translated;
		range: (-5 to: 100).

!!List setting
For this kind of setting, the SettingBrowser will show a drop list. Here is an example for the window position strategy. Notice that the setting is declared by sending the #pickOne message to the builder. Notice also that the list of valid values are given by sending the #domainValues: message to the setting. The argument is an array of association. Each association key is the label that is shown and the corresponding association value gives the value that will be assigned to the preference.
windowPositionStrategySettingsOn: aBuilder
	<systemsettings>
	(aBuilder pickOne: #usedStrategy) 
		label: 'Window position strategy' translated;
		target: RealEstateAgent;
		domainValues: {
			'Reverse Stagger' translated -> #staggerFor:initialExtent:world:. 
			'Cascade' translated -> #cascadeFor:initialExtent:world:. 
			'Standard' translated -> #standardFor:initialExtent:world:};

!!Launcher
A launcher is a particular setting. It allows to launch a script directly from the setting browser. Imagine that you have changed some settings and that you need to evaluate a script in order to update some other objets. It can be used also to configurate globally a package of the entire image.
As an example, in order to use True Type Fonts, the system must be updated by collecting all the available TT fonts. This can be done by evaluating the following expression:
-------------
FreeTypeFontProvider current updateFromSystem
-------------
In order to be able to launch this script from the setting browser, you have to declare a launcher. For example, look-at how the script for the TT fonts is declared in GraphicFontSettings class >> #standardFontsSettingsOn:.

GraphicFontSettings class >> #standardFontsSettingsOn:
	<systemsettings>
	(aBuilder group: #standardFonts)
		...
		(aBuilder launcher: #updateFromSystem)
				order: 1; 
				target: FreeTypeFontProvider;
				targetSelector: #current;
				description: 'Update available fonts by scanning the current system';
				script: #updateFromSystem;
				label: 'Update fonts from system' translated.

Notice that you send #launcher: to the builder in order to create the setting node, then you send #script: to the created node with the selector of the script passed as argument.
</help>

Implementation details
See also SettingNode, SettingTree, SettingNodeBuilder and SettingTreeBuilder classes.

Instance Variables
	roots:		<Collection of SettingTreeNode>
	searchedText:		<String>
	status:		<WriteStream>
	collector:		<SettingCollector>

roots
	- the roots of the viewed setting trees

searchedText
	- the string used as a filter token or regexp in order to select viewed settings

status
	- a WriteStream used in order to store useful information which are shown when no current item is selected

collector
	- The SettingCollector which has the responsibility to collect settings and to build viewed trees

"
Class {
	#name : #SettingBrowser,
	#superclass : #MorphTreeModel,
	#instVars : [
		'viewedPackages',
		'roots',
		'status',
		'searchedText',
		'treeHolder'
	],
	#classInstVars : [
		'searchedTextList',
		'regexpSearch'
	],
	#category : #'System-Settings-Browser'
}

{ #category : #'private helpers' }
SettingBrowser class >> allSettingsState [
	"
	Workspace openContents: self allSettingsState label: 'Current Setting assignments' 
	"
	| declList |
	declList := (self currentNodeList select: [ :node | node item hasValue])
		collect: [ :node | node item ].
	^ String streamContents: [:s | declList do: [:d | d settingStoreOn: s] ]
]

{ #category : #opening }
SettingBrowser class >> browse: aCollectionOfKeywords [
	"open a SettingBrowser view which is able to browse all settings 
	that are declared with a pragma keyword which is at least
	one of the string contained in aCollectionOfKeywords
	example:
	(SettingBrowser browse: #('systemsettings'))
	browse all settings which make use of the pragma keyword 'systemsettings'
	
	Note that the default setting pragma keyword is 'systemsettings'
	"
	(self forKeywords: aCollectionOfKeywords) open
]

{ #category : #'private helpers' }
SettingBrowser class >> currentNodeList [
	^ (SettingTree new  acceptableKeywords: self settingsKeywords) nodeList
]

{ #category : #opening }
SettingBrowser class >> forKeywords: aCollectionOfKeywords [
	"create a new SettingBrowser initialized with a set of pragma keywords"
	^ self new acceptableKeywords: aCollectionOfKeywords
]

{ #category : #accessing }
SettingBrowser class >> initialExtent [
	"Initial extent of the SettingBrowser window"
	^ 900@600
		
]

{ #category : #'world menu' }
SettingBrowser class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Settings)
		parent: #Pharo;
		order: 1;
		iconName: #smallConfigurationIcon;
		action: [ SettingBrowser open ];
		keyText: 'o, s';
		help: 'Opens a SystemSettingBrowser which allows you to alter many system settings.';
		withSeparatorAfter
]

{ #category : #opening }
SettingBrowser class >> open [
	"Open a SettingBrowser view"
	^ self new open
]

{ #category : #updating }
SettingBrowser class >> refreshAllSettingBrowsers [
	self allSubInstances do: [:sb | sb updateList]
]

{ #category : #accessing }
SettingBrowser class >> regexpSearch [
	"Is the regexp filtering used"
	^ regexpSearch ifNil: [regexpSearch := false]
]

{ #category : #accessing }
SettingBrowser class >> regexpSearch: aBoolean [
	"Set to true if the regexp filtering is to be used"

	regexpSearch := aBoolean
]

{ #category : #accessing }
SettingBrowser class >> searchedTextList [
	"Keep a list of previously entered filtering tokens"
	^ searchedTextList ifNil: [searchedTextList := OrderedCollection new]
]

{ #category : #accessing }
SettingBrowser class >> settingsKeywords [ 
	^ ((PragmaCollector new filter: [:prg | prg keyword = #settingPragmaProcessor]) reset) collect: [:p | p method selector].

]

{ #category : #icons }
SettingBrowser class >> taskbarIconName [
	^#smallConfigurationIcon
]

{ #category : #'private helpers' }
SettingBrowser class >> viewAllSettings [
	"self viewAllSettings"
	Smalltalk tools workspace openContents: self allSettingsState label: 'Current Setting assignments' 

]

{ #category : #'help system' }
SettingBrowser class >> wikiStyleHelp [
	<wikiStyleHelp: #'The setting browser'>
	^ self comment
]

{ #category : #menu }
SettingBrowser >> about [
	self inform: 'Here you can browse, query and change all system settings' translated
]

{ #category : #accessing }
SettingBrowser >> acceptableKeywords: aCollectionOfRegExps [ 
	self treeHolder acceptableKeywords: aCollectionOfRegExps 
]

{ #category : #accessing }
SettingBrowser >> addToSearchedTextList: aString [
	(self searchedTextList includes: aString) 
		ifFalse: [self searchedTextList size > 5 ifTrue: [self searchedTextList removeFirst].
		self searchedTextList add: aString]
]

{ #category : #accessing }
SettingBrowser >> addToStatus: aText [ 
	self status nextPutAll: aText.

]

{ #category : #accessing }
SettingBrowser >> allPackagesWithSettings [
	^( self treeHolder nodeList collect: [:n | n receiverPackage]) asSet
]

{ #category : #updating }
SettingBrowser >> applyFilters [
	roots := nil.
	self searchedText
		ifNotEmpty: [self updateAccordingTo: self textFilter].
	self viewedPackages
		ifNotEmpty: [ | filter |
			filter := SettingPackageFilter new
			packages: self viewedPackages.
			self updateAccordingTo: filter].
	self updateList

]

{ #category : #'user interface' }
SettingBrowser >> buildWindowIn: aWindow [ 
	| statusView toolBar treeMorph toolBarY gap packageListView statusViewHeight |
	treeMorph := self treeMorphIn: aWindow.
	statusView := self statusViewIn: aWindow.
	statusViewHeight := 100.
	toolBar := self toolBarIn: aWindow.
	gap := 3.
	packageListView := self packageListViewIn: aWindow.
	toolBarY := toolBar minExtent y + gap.
	aWindow 
		addMorph: toolBar
		fullFrame: ((0 @ 0 corner: 1 @ 0) asLayoutFrame 
						topOffset: gap;
						bottomOffset: toolBarY + gap).
	aWindow 
		addMorph: treeMorph
		fullFrame: ((0 @ 0 corner: 1 @ 0.99) asLayoutFrame 
				topOffset: toolBarY + gap + gap;
				bottomOffset: statusViewHeight negated).
	aWindow 
		addMorph: statusView
		fullFrame: ((0 @ 0.99 corner: 0.75 @ 1) asLayoutFrame 
				topOffset: statusViewHeight negated).
	aWindow 
		addMorph: packageListView
		fullFrame: ((0.75 @ 0.99 corner: 1 @ 1) asLayoutFrame 
				topOffset: statusViewHeight negated).
	^ aWindow
]

{ #category : #updating }
SettingBrowser >> changePackageSet: aSet [
	"
	This method allow for restricting the settings displayed in the browser. 
	Opening a setting browser display all the setting. 
	SettingBrowser new changePackageSet: { (RPackageOrganizer default packageNamed: 'GT-Playground') }; open
	"
	aSet ~= self viewedPackages
		ifTrue: [self viewedPackages: aSet.
			self applyFilters].
	^ true
]

{ #category : #updating }
SettingBrowser >> changeSearchedText: aString [ 
	self searchedText: aString.
	self applyFilters.
	^ true 
]

{ #category : #'user interface' }
SettingBrowser >> choosePackagesIn: aWindow [ 
	"Method called when one presses the 'Choose packages' buttong.
	This button restrict the set of setting to be considered. By selecting one or more packages, only the setting of these packages are displayed."
	| result list |
	list := MorphTreeModel new
				beCheckList;
				autoMultiSelection: true;
				rootItems: (self allPackagesWithSettings asArray sort: [:a :b | a packageName < b packageName]);
				 wrapBlockOrSelector: #packageName.
	result := list openDialogWindowIn: aWindow title: 'Choose packages' selectedtems: self viewedPackages.
	result ifNotNil: [self changePackageSet: result]
]

{ #category : #updating }
SettingBrowser >> closePathFrom: aSetting [
	(self settingNodeOfDeclaration: aSetting)
		ifNotNil: [ :node | self requestView: (MorphTreeChangeRequest collapseNodePath: (node path collect: [ :n | n withoutListWrapper ])) ]
]

{ #category : #menu }
SettingBrowser >> doubleClick [
	self selectedNode ifNotNil: [:node | node browseDeclaration]
]

{ #category : #export }
SettingBrowser >> exportAllSettings: actions by: groupSize withBasename: aString [
	"Export all settings in files. No more than groupSize settings will be exported to the same file. If there are more than groupSize settings, settings will be exported in multiple files (named aString%d.st, with %d a number increasing from 1 to (actions size / groupSize)."
	| index |
	index := 1.
	actions
		groupsOf: groupSize
		atATimeDo: [ :setting | 
			self exportSettings: setting toFileNamed: aString , index printString , '.st'.
			index := index + 1 ].
	self
		exportSettings: (actions last: (actions size rem: groupSize))
		toFileNamed: aString , index printString , '.st'
]

{ #category : #export }
SettingBrowser >> exportSettings [
	| title nodes actions   |
	
	nodes := self treeHolder nodeList.
	title := 'Exporting settings'.
	title
		displayProgressFrom: 1 
		to: nodes size 
		during: [:bar | actions := nodes collectWithIndex: [:e :i | 
											bar current: i.
											bar label: (String streamContents: [:s | 
															s << title << ' (' << (e item label) << ')']).
											e item exportSettingAction ]].
		
	actions := actions reject: [:e | e isNil ].
	self exportAllSettings: actions by: 50 withBasename: 'exported_settings'.
]

{ #category : #export }
SettingBrowser >> exportSettings: aCollection toFileNamed: filename [
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: aCollection named: filename
]

{ #category : #accessing }
SettingBrowser >> getDescription [
	^ (self selectedNode)
		ifNil: [self status contents ifEmpty: ['Hit return in text fields to accept the input' translated]]
		ifNotNil: [:sel | (Text string: sel item label attribute: TextEmphasis bold), String cr, sel item description]
]

{ #category : #'user interface' }
SettingBrowser >> getViewedPackageNames [
	^ Text
		streamContents: [:s | self viewedPackages
				ifEmpty: [s nextPutAll: 'All packages' translated]
				ifNotEmpty: [:selections | 
					(selections asSet = self allPackagesWithSettings asSet)
						ifTrue: [s nextPutAll: 'All packages']
						ifFalse: [selections
								do: [:package | 
									s nextPutAll: package packageName.
									s nextPut: Character space]]]]
]

{ #category : #'user interface' }
SettingBrowser >> helpDeclaringAndManagingSettings [
	"Open up a workspace with explanatory info in it about the Setting Browser"
	UIManager default edit: 'To be documented here later' label: 'Declaring and managing settings' translated. 
	
]

{ #category : #'user interface' }
SettingBrowser >> helpMenuIn: aWindow [
	| helpMenu |
	helpMenu := aWindow newMenu.
	helpMenu
		addToggle: 'About' translated
		target: self
		selector: #about.
	helpMenu lastItem font: aWindow theme menuFont.
	helpMenu
		addToggle: 'Declaring and managing settings' translated
		target: self
		selector: #helpDeclaringAndManagingSettings.
	helpMenu lastItem font: aWindow theme menuFont.
	^ helpMenu

]

{ #category : #'user interface' }
SettingBrowser >> initialExtent [
	^ self class initialExtent
]

{ #category : #'user interface' }
SettingBrowser >> isRendered [
	^ self dependents anySatisfy: [ :dependent | dependent isSystemWindow and: [ dependent owner isNotNil ] ]
]

{ #category : #menu }
SettingBrowser >> keyStroke: anEvent from: aTreeMorph [
	| c |
	c := anEvent keyCharacter.
	c = $a
		ifTrue: [^ self expandAll].
	c = $A
		ifTrue: [^ self collapseAll]. 
	self selectedNode
		ifNotNil: [:current | current keyStroke: anEvent from: aTreeMorph.
			current settingDeclaration hasDefault
				ifTrue: [c = $d
						ifTrue: [^ self menuSetToDefault: current]].
			current settingDeclaration hasEditableList
				ifTrue: [c = $e
						ifTrue: [^ self menuEmptyList: current]]]
]

{ #category : #menu }
SettingBrowser >> loadSettings [
	self treeHolder updateSettingNodes.
	self inform: 'Settings has been updated from the disk.'
]

{ #category : #menu }
SettingBrowser >> menu: menu shifted: b [ 
	"Set up the menu to apply to the receiver's, 
	honoring the #shifted boolean"
	menu
		add: 'Expand all (a)' 
		target: self
		selector: #expandAll. 
	menu
		add: 'Collapse all (A)' 
		target: self 
		selector: #collapseAll.
	self selectedNode
		ifNotNil: [:current | 
			menu
				add: 'Expand all from here' 
				target: self
				selector: #expandAllFromNode:
				argument: current.
			menu addLine.
			current menu: menu shifted: b].
	self selectedNode
		ifNotNil: [:current |
			current settingDeclaration hasDefault
				ifTrue: [
					menu addLine.
					menu
						add: 'Set to default (d)' 
						target: self
						selector: #menuSetToDefault:
						argument: current].
			current settingDeclaration hasEditableList
				ifTrue: [
					current settingDeclaration hasDefault
						ifFalse: [  menu addLine ].
					menu
						add: 'Empty list (e)' 
						target: self
						selector: #menuEmptyList:
						argument: current]].
	^ menu
]

{ #category : #menu }
SettingBrowser >> menuEmptyList: aTreeNode [
	| v |
	(v := aTreeNode settingDeclaration) hasEditableList 
		ifTrue: [v emptyList. self updateList]
]

{ #category : #menu }
SettingBrowser >> menuSetToDefault: aTreeNode [
	| v |
	(v := aTreeNode settingDeclaration) hasDefault
		ifTrue: [(self confirm: 'Set to default value ?' translated)
				ifTrue: [v setToDefault. self updateList]]
]

{ #category : #'user interface' }
SettingBrowser >> newTreeHolder [
	^ SettingTree new  acceptableKeywords: self settingsKeywords
]

{ #category : #'user interface' }
SettingBrowser >> open [
	
	Cursor wait
		showWhile: [ | window |window := StandardWindow new model: self.
			window title: self windowTitle.
			self buildWindowIn: window.
			window themeChanged.
			window openInWorld.
			(window findDeeplyA: self treeMorphClass)  takeKeyboardFocus. 
			^ window]
]

{ #category : #updating }
SettingBrowser >> openPathFrom: aSetting [
	(self settingNodeOfDeclaration: aSetting) ifNotNil: [:node |
		self expandNodePath: node path]
]

{ #category : #'user interface' }
SettingBrowser >> packageListViewIn: aWindow [
	^ aWindow
				newTextEditorFor: self
				getText: #getViewedPackageNames
				setText: #setViewedPackageNames:.

]

{ #category : #accessing }
SettingBrowser >> pragmaErrorOccured: aPragmaError [
	self setDescription: aPragmaError message, String cr
]

{ #category : #accessing }
SettingBrowser >> regexpSearch [
	^ self class regexpSearch
]

{ #category : #accessing }
SettingBrowser >> regexpSearch: aBoolean [
	self class regexpSearch: aBoolean.

]

{ #category : #'initialize-release' }
SettingBrowser >> release [
	treeHolder release.
	treeHolder := nil.
	roots := nil.
	status := nil.
	searchedText := nil.
	viewedPackages := nil.
	super release.
]

{ #category : #accessing }
SettingBrowser >> rootNodes [
	^ roots ifNil: [roots := self treeHolder settingTreeRoots]

]

{ #category : #accessing }
SettingBrowser >> searchedText [
	^ searchedText ifNil: [searchedText := '']
]

{ #category : #accessing }
SettingBrowser >> searchedText: aString [
	searchedText := aString.

]

{ #category : #accessing }
SettingBrowser >> searchedTextList [
	^ self class searchedTextList
]

{ #category : #updating }
SettingBrowser >> selection: aSelectionHolder [
	super selection: aSelectionHolder.
	self setDescription: ''.
	self changed: #getDescription

]

{ #category : #menu }
SettingBrowser >> setAllToDefault [
	(self confirm: 'Set all to default value?' translated) ifTrue: [
		self treeHolder nodeList do: [:node | node item hasDefault ifTrue: [node item setToDefault]].
		self updateList]
]

{ #category : #accessing }
SettingBrowser >> setDescription: aText [ 
	self status reset.
	self addToStatus: aText.
	self changed: #getDescription
]

{ #category : #'user interface' }
SettingBrowser >> setViewedPackageNames: aText [ 
	| allViewed |
	allViewed := Set new.
	aText asString substrings
		do: [:sub | (RPackageOrganizer default
						packageNamed: sub
						ifAbsent: [])
				ifNotNil: [:pkg | allViewed add: pkg]].
	self changePackageSet: allViewed.
	self changed: #getViewedPackageNames

]

{ #category : #accessing }
SettingBrowser >> settingNodeOfDeclaration: aSettingDeclaration [
	^ self treeHolder deeplyDetect: [:n | n settingDeclaration = aSettingDeclaration]
]

{ #category : #updating }
SettingBrowser >> settingValueChanged: aSetting [ 
	aSetting realValue = true
		ifTrue: [self openPathFrom: aSetting].
	aSetting realValue = false
		ifTrue: [self closePathFrom: aSetting].
	aSetting changed: #realValue.
	aSetting changed: #fontButtonLabel.
	aSetting changed: #index.
	aSetting changed: #content 
]

{ #category : #'user interface' }
SettingBrowser >> settingsKeywords [ 
	^ self class settingsKeywords
]

{ #category : #accessing }
SettingBrowser >> status [
	^ status ifNil: [status := String new writeStream]
]

{ #category : #'user interface' }
SettingBrowser >> statusViewIn: aMorph [
	| statusView |
	statusView := aMorph
				newTextEditorFor: self
				getText: #getDescription
				setText: #setDescription:.
	^ statusView
 
]

{ #category : #menu }
SettingBrowser >> storeSettings [
	self treeHolder storeSettingNodes.
	self inform: 'Settings has been stored on the disk.'
]

{ #category : #updating }
SettingBrowser >> textFilter [
	^ self regexpSearch
		ifFalse: [SettingTokenFilter new token: self searchedText]
		ifTrue: [SettingRegexpFilter new regexp: self searchedText] 
]

{ #category : #'user interface' }
SettingBrowser >> toolBarIn: aMorph [ 
	| toolBar |
	toolBar := aMorph newToolbar: {
	aMorph
		newButtonFor: self
		getState: nil
		action: #expandAll
		arguments: nil
		getEnabled: nil
		label: 'Expand all' translated
		help: 'Expand all tree nodes' translated.
	aMorph newToolSpacer.
	aMorph newToolSpacer.
	aMorph newLabel: 'Search for: ' translated.
	(SearchMorph
				on: self
				list: #searchedTextList
				selected: #searchedText
				changeSelected: #changeSearchedText:
				useIndex: false 
				addToList: #addToSearchedTextList:
				class: String
				getEnabled: nil) hResizing: #spaceFill; 
				ghostText: 'Hit return to accept' translated.
	aMorph
			newCheckboxFor: self
			getSelected: #regexpSearch
			setSelected: #regexpSearch:
			getEnabled: nil
			label: 'Regexp' 
			help: 'Filter according to a regular expression' translated.
	aMorph newToolSpacer.	
	aMorph newToolSpacer.
	(aMorph
				newButtonFor: self
				getState: nil
				action: #choosePackagesIn:
				arguments: {aMorph}
				getEnabled: nil
				label: 'Choose packages' translated 
				help: 'Per default, SettingBrowser shows all the setting of the system. You can restrict what is displayed to some packages that contains setting defintions.' translated).
	(aMorph
				newButtonFor: self
				getState: nil
				action: #storeSettings
				arguments: nil
				getEnabled: nil
				label: 'Store Settings' translated 
				help: 'Store the current settings' translated).
	(aMorph
				newButtonFor: self
				getState: nil
				action: #loadSettings
				arguments: nil
				getEnabled: nil
				label: 'Load Settings' translated 
				help: 'Load the current settings from the disk' translated).
	aMorph newToolSpacer}.
	^ toolBar
]

{ #category : #updating }
SettingBrowser >> treeChanged: anAnnounce [
	roots := nil.
	self selection: nil.
	self updateList
]

{ #category : #accessing }
SettingBrowser >> treeHolder [
	^ treeHolder
		ifNil: [treeHolder := self newTreeHolder whenChangedSend: #treeChanged: to: self; yourself]
]

{ #category : #'user interface' }
SettingBrowser >> treeMorphIn: aMorph [ 
	| treeMorph |
	treeMorph := (self treeMorphClass on: self)
		getMenuSelector: #menu:shifted:;
		keystrokeActionSelector: #keyStroke:from:;
		columns: {
			MorphTreeColumn new startWidth: 300; rowMorphGetSelector: #labelMorphFor:; yourself.
			MorphTreeColumn new rowMorphGetSelector: #inputMorphFor:;yourself
		};
		doubleClickSelector: #doubleClick;
		rowInset: 6;
		columnInset: 6;
		rowColorForEven: Smalltalk ui theme lightBackgroundColor;
		yourself.
	^ treeMorph buildContents
]

{ #category : #updating }
SettingBrowser >> updateAccordingTo: aFilter [
	self selection: nil.
	self setDescription: ''.
	roots := nil.
	self updateList.
	[
	self rootNodes do: [:r | r updateAccordingTo: aFilter].
	self rootNodes removeAllSuchThat: [:n | (aFilter keepSettingTreeNode: n) not and: [n isEmpty]].
	]
		on: RegexSyntaxError
		do: [:ex | 
			self setDescription: 'Regular expression syntax error, see `documentation'' protocol of RxParser class for user''s guide'].
	self expandAll
]

{ #category : #updating }
SettingBrowser >> updateList [
	self isRendered ifTrue: [
		self treeHolder updateList.
		self changed: #getDescription.
		self changed: #getViewedPackageNames.
		super updateList].	

]

{ #category : #updating }
SettingBrowser >> updateStatusWith: aMessage [
	self addToStatus: aMessage.
	self addToStatus: Character cr.

]

{ #category : #accessing }
SettingBrowser >> viewedLabelOfSetting: aSettingDeclaration [ 
	| node |
	node := self settingNodeOfDeclaration: aSettingDeclaration.
	^ node 
		ifNil: [aSettingDeclaration label]
		ifNotNil: [(node allChildren notEmpty and: [aSettingDeclaration hasValue])
				ifTrue: [aSettingDeclaration label, '...'] 
				ifFalse: [aSettingDeclaration label]]
]

{ #category : #accessing }
SettingBrowser >> viewedPackages [
	^ viewedPackages
		ifNil: [viewedPackages := Set new]
]

{ #category : #accessing }
SettingBrowser >> viewedPackages: aCollectionOfPackagesOrNil [
	viewedPackages := aCollectionOfPackagesOrNil
]

{ #category : #'user interface' }
SettingBrowser >> windowTitle [
	^ 'Settings Browser'
]

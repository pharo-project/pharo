"
i'm a UI for show you how the setting of prettyPrinter(code formater ) will be applied on method not formated

HOW TO RUN 
 BISettingPreviewer open

customization 
	see protocol customization and use those method

ROADMAP
-------

- [X] 1st step migration to Spec 2 
	- [X] add a set of method to format and add/remove to the set
	- [X] show before and after format
	- [x] add a set of setting (context) to configure formater 
- [ ] 2nd step 
	- [ ] extract conduct of SettingDeclaration ( one part setting , other part visitor  ) see DEV part , settingTree
- [ ] 3rd step 
	- [ ] when click on a specific setting show you some example with this setting to understand how ti's work
README DEV & USER 
-----------------

be carefull of settingDropList and the table of setting customization each time you modified something it's save into system.

USER Part 
---------
i'm composed of : 
1) a tree of setting each row it's like this (name of a setting , one or more input to customize them ) 
2) 
	2.A) 3 buttons
		a) save button it save what you write in the textArea below 
		b) remove Current Method it's remove the dropList selectedItem
		c) call another UI to select another method in the system 
	2.B ) the methodDropList it's a dropList to switch method to show you how it will be formate
		each time you change the selectedItem it will change the content of textArea before formate and also the after formate textArea 
	2.C) it's a textArea to show the method to format
3) 
	3.A) 2  buttons
		a) the first one call 'save' it's use to save the configuration of the formater it will ask you a name for the method and it will appear in the setting dropList
		b) the remove Button it's remove selectedItem of Setting DropList 
	3.B) it's the setting dropList each time you change it refresh all the textArea
	3.C) textArea to show formate Code 
DEV part
-------- 

##############
MethodProvider 
##############

this UI use a reference to another class 'instance variable : methodProvider'
	in this class 
		- class side we store all setting at format STON with tag 'settings'
		- instance side we store all method to format with tag: 'methods'

###########
settingTree
###########

the settingTree is write in morph for the moment it will take me too much to migrate it to Spec 2 
the problem is :
	the setting is higly link to morph 
	in the SettingDeclaration there is a method get an input/widget to customize with someting like a visitor.I think to migrate this part we will have to extract this feature with a visitor
	LINK:
	SettingDeclaration >> inputWidget , it's the method like a Visitor
	
#############
other widgets
#############

nothing to say  

Internal Representation and Key Implementation Points.

    Instance Variables
	afterPrettyPrintTextPresenter:		<CodePresenter>
	beforePrettyPrintTextPresenter:		<CodePresenter>
	chooseMethodUI:		<ButtonPresenter>
	formaterPrettyPrinter:		<BIConfigurableFormatter>
	isSettingModification:		<Boolean>
	methodDropList:		<DropListPresenter>
	methodProvider:		<Object> reference to another class 
	removeMethodButton:		<ButtonPresenter>
	removeSettingButton:		<buttonPresenter>
	saveMethodButton:		<buttonPresenter>
	saveSettingButton:		<buttonPresenter>
	settingsDropList:		<DropListPresenter>
	settingsTree:		<TablePresenter>

"
Class {
	#name : #BISettingPreviewer,
	#superclass : #ComposablePresenter,
	#instVars : [
		'settingsTree',
		'chooseMethodUI',
		'methodDropList',
		'beforePrettyPrintTextPresenter',
		'afterPrettyPrintTextPresenter',
		'settingsDropList',
		'saveMethodButton',
		'removeMethodButton',
		'removeSettingButton',
		'saveSettingButton',
		'methodProvider',
		'formaterPrettyPrinter',
		'isSettingModification'
	],
	#classVars : [
		'FormaterPrettyPrinter',
		'MethodProvider'
	],
	#category : #'BlueInk-Extras'
}

{ #category : #'build ui buttons' }
BISettingPreviewer class >> buildOpenBIInspectorButton [
	^ PluggableButtonMorph
		on: self
		getState: #openBIInspectorButtonState
		action: #openBIInspectorButtonAction
		label: #openBIInspectorButtonLabel
]

{ #category : #specs }
BISettingPreviewer class >> defaultSpec [
	<spec: #default>
	^ SpecBoxLayout newVertical
		add: #settingsTree; add:
			(SpecPanedLayout newHorizontal
				add:
					(SpecBoxLayout newVertical
						add:
							(SpecBoxLayout newHorizontal
								add: #saveMethodButton; add: #removeMethodButton; add: #chooseMethodUI; 
									yourself)
							withConstraints: [ :contraint | contraint height: 30 ]; add: #methodDropList
							withConstraints: [ :contraint | contraint height: 30 ]; add: #beforePrettyPrintTextPresenter; 
							yourself); add:
					(SpecBoxLayout newVertical
						add:
							(SpecBoxLayout newHorizontal
								add: #saveSettingButton; add: #removeSettingButton; yourself)
							withConstraints: [ :contraint | contraint height: 30 ]; add: #settingsDropList
							withConstraints: [ :contraint | contraint height: 30 ]; add: #afterPrettyPrintTextPresenter; 
							yourself); yourself); yourself
]

{ #category : #settings }
BISettingPreviewer class >> dialogOpenBIPreviewer [
	^ Smalltalk ui theme
		newRowIn: World
		for:
			{(Smalltalk ui theme buttonLabelForText: 'Open Blue Ink Setting Previewer ').
			self buildOpenBIInspectorButton}
]

{ #category : #'instance creation' }
BISettingPreviewer class >> initialize [
	MethodProvider := BIMethodProvider.
	FormaterPrettyPrinter := BIConfigurableFormatter new
]

{ #category : #accessing }
BISettingPreviewer class >> methodProvider [
	^ MethodProvider
]

{ #category : #example }
BISettingPreviewer class >> open [
	<script>
	| presenter |
	presenter := self new.
	presenter openWithSpec.
	^ presenter
]

{ #category : #'button behavior' }
BISettingPreviewer class >> openBIInspectorButtonAction [
	<script>
	self open
]

{ #category : #'button behavior' }
BISettingPreviewer class >> openBIInspectorButtonLabel [
	^ 'Open'
]

{ #category : #'button behavior' }
BISettingPreviewer class >> openBIInspectorButtonState [
	^ true
]

{ #category : #'instance creation' }
BISettingPreviewer class >> openOnClass: aClass [
	| presenter |
	presenter := self new.
	presenter methodProvider: aClass presenter openWithSpec.
	^ presenter
]

{ #category : #settings }
BISettingPreviewer class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #blueInkFormatterExtra)
		target: self;
		parent: #blueInkFormatter;
		label: 'BlueInk Formatting Editor';
		description: 'Open the BlueInk setting previewer to edit the formatting options';
		dialog: [ self dialogOpenBIPreviewer ]

	
]

{ #category : #visiting }
BISettingPreviewer >> acceptActionBIChooseMethod: anInstanceOFBIChooseMethodUI [
	anInstanceOFBIChooseMethodUI methodList selection selectedItem
		ifNil: [ self inform: 'you have to select a method' ]
		ifNotNil:
			[ :selectedItem | beforePrettyPrintTextPresenter text: selectedItem sourceCode ]
]

{ #category : #visiting }
BISettingPreviewer >> acceptActionBICodePresenter: aBICodePresenter [
	| string newSettingMethod |
	isSettingModification := true.
	string := String
		streamContents: [ :stream | 
			stream
				nextPutAll: aBICodePresenter codePresenter text;
				cr;
				tab;
				nextPutAll: '^ '.
			(STON
				toStringPretty: formaterPrettyPrinter class defaultPrettyPrintContext)
				storeOn: stream ].
	newSettingMethod := methodProvider class
		>> (methodProvider class compile: string).
	newSettingMethod protocol: 'settings'.
	settingsDropList
		items:
			(settingsDropList listItems
				removeAllSuchThat: [ :item | item selector = newSettingMethod selector ];
				add: newSettingMethod;
				yourself).
	isSettingModification := false
]

{ #category : #'initialization-actions' }
BISettingPreviewer >> actionMethodDropList [
	methodDropList
		transmitTo: beforePrettyPrintTextPresenter
		transform: [ :method | method sourceCode ].
	methodDropList
		transmitTo: afterPrettyPrintTextPresenter
		transform: [ :item | self formatPrettyPrint ]
]

{ #category : #'initialization-actions' }
BISettingPreviewer >> actionRemoveMethodButton [
	removeMethodButton
		action: [ | selectedItem |
			selectedItem := methodDropList selectedItem.
			selectedItem removeFromSystem.
			self updateAllWidgets ]
]

{ #category : #'initialization-actions' }
BISettingPreviewer >> actionRemoveSettingButton [
	removeSettingButton
		action: [ isSettingModification := true.
			settingsDropList selectedItem
				ifNil: [ self inform: 'please select a confguration' ]
				ifNotNil: [ :selectedItem | 
					selectedItem removeFromSystem.
					settingsDropList selectedIndex: 1.
					settingsDropList
						items:
							(settingsDropList listItems
								remove: selectedItem;
								yourself)
					].
			isSettingModification := false
			]
]

{ #category : #'initialization-actions' }
BISettingPreviewer >> actionSaveMethodButton [
	saveMethodButton
		action: [ | newMethod |
			newMethod := methodProvider
				>> (methodProvider compile: beforePrettyPrintTextPresenter text).
			newMethod protocol: 'methods'.
			methodDropList selectedIndex: 1
			]
]

{ #category : #'initialization-actions' }
BISettingPreviewer >> actionSaveSettingButton [
	saveSettingButton action: [ BIInputPresenterUI openOn: self ]
]

{ #category : #'initialization-actions' }
BISettingPreviewer >> actionSettingDropList [
	settingsDropList
		whenSelectionChangedDo: [ :setting | 
			formaterPrettyPrinter
				installNewContext:
					(STON
						fromString:
							(setting selectedItem model
								valueWithReceiver: methodProvider
								arguments: #())).
			afterPrettyPrintTextPresenter text: self formatPrettyPrint
			]
]

{ #category : #accessing }
BISettingPreviewer >> afterPrettyPrintTextPresenter [
	^ afterPrettyPrintTextPresenter
]

{ #category : #accessing }
BISettingPreviewer >> beforePrettyPrintTextPresenter [
	^ beforePrettyPrintTextPresenter
]

{ #category : #accessing }
BISettingPreviewer >> chooseMethodUI [
	^ chooseMethodUI
]

{ #category : #initialization }
BISettingPreviewer >> defaultMethodProvider [
	methodProvider := self class methodProvider 
]

{ #category : #'item creation' }
BISettingPreviewer >> displayNodeFor: aNode [
	| nodeMorphLeft nodeMorphRight |
	nodeMorphLeft := StringMorph contents: aNode item label.
	nodeMorphRight := (self theme newRowIn: World for: {aNode settingDeclaration inputWidget})
		clipSubmorphs: true;
		cellInset: 0;
		width: 570;
		left: 400;
		yourself.
	^ PanelMorph new
		addAllMorphs:
			{nodeMorphLeft.
			nodeMorphRight}; 
		color: Smalltalk ui theme lightBackgroundColor;
		hResizing: #shrinkWrap;
		yourself
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> formatPrettyPrint [
	^ formaterPrettyPrinter format: methodDropList selectedItem ast
]

{ #category : #accessing }
BISettingPreviewer >> formaterPrettyPrinter [
	^ formaterPrettyPrinter
]

{ #category : #customization }
BISettingPreviewer >> formaterPrettyPrinter: aPrettyPrinter [
	formaterPrettyPrinter := aPrettyPrinter 
]

{ #category : #utilities }
BISettingPreviewer >> getMethodsToPrettyPrint [
	^ methodProvider methods
		select: [ :each | each category = 'methods' ]
]

{ #category : #utilities }
BISettingPreviewer >> getSettings [
	^ (methodProvider methods , methodProvider class methods)
		asOrderedCollection select: [ :each | each category = 'settings' ]
]

{ #category : #api }
BISettingPreviewer >> initialExtent [
	^ 1000 @ 700
]

{ #category : #initialization }
BISettingPreviewer >> initialize [
	super initialize.
	isSettingModification := false.
]

{ #category : #initialization }
BISettingPreviewer >> initializePresenter [
	self actionMethodDropList.
	self actionRemoveMethodButton.
	self actionSaveMethodButton.
	self actionRemoveSettingButton.
	self actionSaveSettingButton.
	self actionSettingDropList
]

{ #category : #initialization }
BISettingPreviewer >> initializeWidgets [
	methodProvider := MethodProvider.
	formaterPrettyPrinter := FormaterPrettyPrinter.
	settingsTree := self newTree.
	methodDropList := self newDropList.
	chooseMethodUI := self newButton.
	beforePrettyPrintTextPresenter := self newCode.
	afterPrettyPrintTextPresenter := self newCode.
	settingsDropList := self newDropList.
	saveMethodButton := self newButton.
	removeMethodButton := self newButton.
	saveSettingButton := self newButton.
	removeSettingButton := self newButton.
	self setupSaveMethodButton.
	self setupRemoveMethodButton.
	self setupRemoveSettingButton.
	self setupSaveSettingButton.
	self setupMethodDropList.
	self setupSettingsDropList.
	self setupSettingsTree.
	self setupChooseMethodUI.
	self setupBeforePrettyPrinterCodePresenter.
	self setupAfterPrettyPrinterCodePresenter.
	self unregisterAnnouncement.
	SystemAnnouncer uniqueInstance weak
		when: MethodAdded send: #methodAdded: to: self;
		when: MethodRemoved send: #methodRemoved: to: self;
		when: MethodModified send: #methodModified: to: self.
	self announcer when: WindowClosed send: #onWindowClosed to: self.
	self focusOrder
		add: settingsTree;
		add: chooseMethodUI;
		add: methodDropList;
		add: beforePrettyPrintTextPresenter;
		add: afterPrettyPrintTextPresenter
]

{ #category : #'system-Announcement' }
BISettingPreviewer >> methodAdded: aMethodAdded [
	isSettingModification ifNil: [ isSettingModification := false ].
	(isSettingModification not
		and: [ aMethodAdded methodOrigin = methodProvider
				or: [ aMethodAdded methodOrigin = methodProvider class ] ])
		ifFalse: [ ^ self ].
	methodDropList
		items:
			(methodDropList listItems
				add: aMethodAdded method;
				yourself)
]

{ #category : #accessing }
BISettingPreviewer >> methodDropList [
	^ methodDropList
]

{ #category : #'system-Announcement' }
BISettingPreviewer >> methodModified: aModifiedMethod [
	(aModifiedMethod methodOrigin = methodProvider
		and: [ isSettingModification not ])
		ifFalse: [ ^ self ].
	methodDropList
		items:
			(methodDropList listItems
				removeAllSuchThat: [ :method | method selector = aModifiedMethod method selector ];
				add: aModifiedMethod method;
				yourself)
]

{ #category : #accessing }
BISettingPreviewer >> methodProvider [
	^ methodProvider
]

{ #category : #customization }
BISettingPreviewer >> methodProvider: anObject [
	methodProvider := anObject.
	settingsDropList items: self getSettings.
	methodDropList items: self getMethodsToPrettyPrint
]

{ #category : #'system-Announcement' }
BISettingPreviewer >> methodRemoved: aMethodRemoved [
	(aMethodRemoved methodOrigin = methodProvider
		and: [ isSettingModification not ])
		ifFalse: [ ^ self ].
	methodDropList selectedIndex: 1.
	methodDropList
		items:
			(methodDropList listItems
				removeAllSuchThat: [ :method | method selector = aMethodRemoved method selector ];
				yourself)
]

{ #category : #'system-Announcement' }
BISettingPreviewer >> onWindowClosed [
	self unregisterAnnouncement.
]

{ #category : #accessing }
BISettingPreviewer >> removeMethodButton [
	^ removeMethodButton
]

{ #category : #accessing }
BISettingPreviewer >> removeSettingButton [
	^ removeSettingButton
]

{ #category : #accessing }
BISettingPreviewer >> saveMethodButton [
	^ saveMethodButton
]

{ #category : #accessing }
BISettingPreviewer >> saveSettingButton [
	^ saveSettingButton
]

{ #category : #accessing }
BISettingPreviewer >> settingsDropList [
	^ settingsDropList
]

{ #category : #accessing }
BISettingPreviewer >> settingsTree [
	^ settingsTree
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupAfterPrettyPrinterCodePresenter [
	afterPrettyPrintTextPresenter withSyntaxHighlight.
	(methodDropList selectedItem isNil
		or: [ settingsDropList selectedItem isNil ])
		ifTrue: [ ^ self ].
	afterPrettyPrintTextPresenter text: self formatPrettyPrint
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupBeforePrettyPrinterCodePresenter [
	beforePrettyPrintTextPresenter
		withSyntaxHighlight; autoAccept: true.
	methodDropList selectedItem ifNil: [ ^ self ].
	beforePrettyPrintTextPresenter
		text: methodDropList selectedItem sourceCode
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupChooseMethodUI [
	chooseMethodUI
		label: 'choose another method';
		action: [ SpChooseMethodUI openOn: self ]
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupMethodDropList [
	methodDropList
		items:
			self getMethodsToPrettyPrint asOrderedCollection;
		displayBlock: [ :item | item selector ].
	methodDropList listItems ifNil: [ ^ self ].
	methodDropList selectedIndex: 1
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupRemoveMethodButton [
	removeMethodButton label: 'remove current Method'
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupRemoveSettingButton [
	removeSettingButton label: 'remove setting'
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupSaveMethodButton [
	saveMethodButton label: 'save'
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupSaveSettingButton [
	saveSettingButton label: 'save'
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupSettingsDropList [
	| selectedSetting |
	settingsDropList
		items: self getSettings;
		displayBlock: [ :each | each selector ].
	settingsDropList listItems ifNil: [ ^ self ].
	settingsDropList selectedIndex: 1.
	selectedSetting := settingsDropList selectedItem.
	formaterPrettyPrinter
		installNewContext:
			(STON
				fromString:
					(selectedSetting
						valueWithReceiver: selectedSetting origin
						arguments: #()))
]

{ #category : #'initialization widgets' }
BISettingPreviewer >> setupSettingsTree [
	| parentName |
	(formaterPrettyPrinter class respondsTo: #settingGroup)
		ifFalse: [ parentName := #blueInkFormatter ]
		ifTrue: [ parentName := formaterPrettyPrinter class settingGroup ].
	settingsTree
		roots:
			((SettingTree acceptableKeywords: #(#systemsettings)) nodeList
				select: [ :node | node parentName = parentName ])
				asOrderedCollection;
		displayBlock: [ :node | self displayNodeFor: node ]
]

{ #category : #api }
BISettingPreviewer >> title [
	^ 'Blue Ink Setting Previewer'
]

{ #category : #'system-Announcement' }
BISettingPreviewer >> unregisterAnnouncement [
SystemAnnouncer uniqueInstance unsubscribe: self  
]

{ #category : #initialization }
BISettingPreviewer >> updateAllWidgets [
	self needRebuild: true.
	^ self buildWithSpec
]

{ #category : #'event handling' }
BISettingPreviewer >> whenASettingChanged [
	afterPrettyPrintTextPresenter text: self formatPrettyPrint 
]

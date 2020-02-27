"
i'm a UI for show you how the setting of prettyPrinter(code formater ) will be applied on method not formated

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
	#name : #EFSettingPreviewer2,
	#superclass : #SpPresenter,
	#instVars : [
		'settingsTree',
		'chooseMethodUI',
		'methodDropList',
		'settingsDropList',
		'saveMethodButton',
		'removeMethodButton',
		'removeSettingButton',
		'saveSettingButton',
		'methodProvider',
		'formaterPrettyPrinter',
		'isSettingModification',
		'formatedTextPresenter',
		'notFormatedTextPresenter'
	],
	#classVars : [
		'FormaterPrettyPrinter',
		'MethodProvider'
	],
	#category : #EnlumineurFormatterUI
}

{ #category : #'build ui buttons' }
EFSettingPreviewer2 class >> buildOpenBIInspectorButton [
	^ PluggableButtonMorph
		on: self
		getState: #openBIInspectorButtonState
		action: #openBIInspectorButtonAction
		label: #openBIInspectorButtonLabel
]

{ #category : #specs }
EFSettingPreviewer2 class >> defaultSpec [
	<spec: #default>
	^ SpBoxLayout newVertical
		add: #settingsTree;
		add:
			(SpPanedLayout newHorizontal
				add:
					(SpBoxLayout newVertical
						add:
							(SpBoxLayout newHorizontal
								add: #saveMethodButton;
								add: #removeMethodButton;
								add: #chooseMethodUI;
								yourself)
							withConstraints: [ :contraint | contraint height: 30 ];
						add: #methodDropList
							withConstraints: [ :contraint | contraint height: 30 ];
						add: #notFormatedTextPresenter;
						yourself);
				add:
					(SpBoxLayout newVertical
						add:
							(SpBoxLayout newHorizontal
								add: #saveSettingButton;
								add: #removeSettingButton;
								yourself)
							withConstraints: [ :contraint | contraint height: 30 ];
						add: #settingsDropList
							withConstraints: [ :contraint | contraint height: 30 ];
						add: #formatedTextPresenter;
						yourself);
				yourself);
		yourself
]

{ #category : #settings }
EFSettingPreviewer2 class >> dialogOpenBIPreviewer [
	^ Smalltalk ui theme
		newRowIn: World
		for:
			{(Smalltalk ui theme buttonLabelForText: 'Open Setting Previewer ').
			self buildOpenBIInspectorButton}
]

{ #category : #'class initialization' }
EFSettingPreviewer2 class >> initialize [
	"self initialize"
	FormaterPrettyPrinter := EFFormatter new.
	MethodProvider := EFMethodProvider
]

{ #category : #'instance creation' }
EFSettingPreviewer2 class >> open [
	<script>
	| presenter |
	presenter := self new.
	presenter openWithSpec.
	^ presenter
]

{ #category : #'button behavior' }
EFSettingPreviewer2 class >> openBIInspectorButtonAction [
	<script>
	self open
]

{ #category : #'button behavior' }
EFSettingPreviewer2 class >> openBIInspectorButtonLabel [
	^ 'Open'
]

{ #category : #'button behavior' }
EFSettingPreviewer2 class >> openBIInspectorButtonState [
	^ true
]

{ #category : #settings }
EFSettingPreviewer2 class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #EnlumineurFormatterExtra)
		target: self;
		parent: #codeFormatters;
		label: 'Formatter Previewer';
		description: 'Open the setting previewer to edit the formatting options';
		dialog: [ self dialogOpenBIPreviewer ]

	
]

{ #category : #specs }
EFSettingPreviewer2 class >> specLayoutHHizontal [
    ^ SpPanedLayout newHorizontal
        add:
            (SpBoxLayout newVertical
                add:
                    (SpBoxLayout newVertical
                        add:
                            (SpBoxLayout newHorizontal
                                add: #saveSettingButton;
                                add: #removeSettingButton;
                                yourself)
                            withConstraints: [ :contraint | contraint height: 30 ];
                        add: #settingsDropList
                            withConstraints: [ :contraint | contraint height: 30 ];
                        yourself);
                add: #settingsTree;
                yourself);
        add:
            (SpBoxLayout newVertical
                add:
                    (SpBoxLayout newHorizontal
                        add: #saveMethodButton;
                        add: #removeMethodButton;
                        add: #chooseMethodUI;
                        yourself)
                    withConstraints: [ :contraint | contraint height: 30 ];
                add: #methodDropList
                    withConstraints: [ :contraint | contraint height: 30 ];
                add: #notFormatedTextPresenter;
                add: #formatedTextPresenter;
                yourself);
        yourself
]

{ #category : #visiting }
EFSettingPreviewer2 >> acceptActionBIChooseMethod: anInstanceOFBIChooseMethodUI [
	anInstanceOFBIChooseMethodUI methodList selection selectedItem
		ifNil: [ self inform: 'you have to select a method' ]
		ifNotNil:
			[ :selectedItem | notFormatedTextPresenter text: selectedItem sourceCode ]
]

{ #category : #visiting }
EFSettingPreviewer2 >> acceptActionBICodePresenter: aBICodePresenter [
	"I'm compile a method returning a String at Ston format of the context"

	| string newSettingMethod |
	isSettingModification := true.
	string := String streamContents: [ :stream | 
		          stream
			          nextPutAll: aBICodePresenter codePresenter text;
			          cr;
			          tab;
			          nextPutAll: '^ '.
		          (STON toStringPretty:
			           formaterPrettyPrinter class defaultPrettyPrintContext) 
			          storeOn: stream ].
	newSettingMethod := methodProvider class
	                    >> (methodProvider class compile: string).
	newSettingMethod protocol: 'settings'.
	settingsDropList items: (settingsDropList listItems
			 removeAllSuchThat: [ :item | 
				 item selector = newSettingMethod selector ];
			 add: newSettingMethod;
			 yourself).
	settingsDropList selectIndex:
		((settingsDropList listItems collect: #selector) indexOf:
			 newSettingMethod selector).
	isSettingModification := false
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> actionChooseMethodUI [
    chooseMethodUI action: [ SpChooseMethodUI openOn: self ]
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> actionMethodDropList [
	methodDropList
		transmitTo: notFormatedTextPresenter
		transform: [ :method | method sourceCode ].
	methodDropList
		transmitTo: formatedTextPresenter
		transform: [ :item | self formatPrettyPrint ]
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> actionRemoveMethodButton [
	removeMethodButton
		action: [ methodDropList selectedItem removeFromSystem ]
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> actionRemoveSettingButton [
	removeSettingButton
		action: [ isSettingModification := true.
			settingsDropList selectedItem
				ifNil: [ self inform: 'please select a confguration' ]
				ifNotNil: [ :selectedItem | 
					selectedItem removeFromSystem.
					settingsDropList selectIndex: 1.
					settingsDropList
						items:
							(settingsDropList listItems
								remove: selectedItem;
								yourself) ].
			isSettingModification := false ]
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> actionSaveMethodButton [
	saveMethodButton action: [ 
		| newMethod |
		newMethod := methodProvider
		             >>
		             (methodProvider compile: notFormatedTextPresenter text).
		newMethod protocol: 'methods'.
		methodDropList selectIndex:
			((methodDropList listItems collect: #selector) indexOf:
				 newMethod selector) ]
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> actionSaveSettingButton [
	saveSettingButton action: [ EFInputPresenterUI openOn: self ]
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> actionSettingDropList [
    settingsDropList
        whenSelectionChangedDo: [ :setting | 
            | currentContext |
            currentContext := STON
                fromString:
                    (setting selectedItem model
                        valueWithReceiver: methodProvider
                        arguments: #()).
            formaterPrettyPrinter installNewContext: currentContext.
            formatedTextPresenter text: self formatPrettyPrint.
            formaterPrettyPrinter class
                defaultPrettyPrintContext: currentContext.
            settingsTree roots: self getSystemsettingsAsParentName ]
]

{ #category : #accessing }
EFSettingPreviewer2 >> chooseMethodUI [
	^ chooseMethodUI
]

{ #category : #'initialization-actions' }
EFSettingPreviewer2 >> foo [ 
	^ 1 + 2 * 3
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> formatPrettyPrint [
	methodDropList selectedItem
		ifNil: [ ^ self ].
	^ formaterPrettyPrinter format: methodDropList selectedItem ast
]

{ #category : #accessing }
EFSettingPreviewer2 >> formatedTextPresenter [
	^ formatedTextPresenter
]

{ #category : #accessing }
EFSettingPreviewer2 >> formaterPrettyPrinter [
	^ formaterPrettyPrinter
]

{ #category : #api }
EFSettingPreviewer2 >> formaterPrettyPrinter: aPrettyPrinter [ 
	formaterPrettyPrinter := aPrettyPrinter.
	settingsTree roots: self getSystemsettingsAsParentName
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> getSystemsettingsAsParentName [
	^ ((SettingTree acceptableKeywords: #(#systemsettings)) nodeList
		select: [ :node | node parentName = formaterPrettyPrinter class settingGroup ])
		asOrderedCollection
]

{ #category : #initialization }
EFSettingPreviewer2 >> initialize [
	super initialize.
	isSettingModification := false.
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> initializeButtons [
	
	

	saveSettingButton label: 'save'.
	chooseMethodUI label: 'choose another method'
]

{ #category : #initialization }
EFSettingPreviewer2 >> initializePresenter [
    self actionMethodDropList.
    self actionRemoveMethodButton.
    self actionSaveMethodButton.
    self actionRemoveSettingButton.
    self actionSaveSettingButton.
    self actionSettingDropList.
    self actionChooseMethodUI
]

{ #category : #initialization }
EFSettingPreviewer2 >> initializeWidgets [
    methodProvider := MethodProvider.
    formaterPrettyPrinter := FormaterPrettyPrinter.
    settingsTree := self newTreeTable.
    methodDropList := self newDropList.
    chooseMethodUI := self newButton.
	chooseMethodUI label: 'choose another method'.
    notFormatedTextPresenter := self newCode.
    formatedTextPresenter := self newCode.
    settingsDropList := self newDropList.
    saveMethodButton := self newButton.
	saveMethodButton label: 'save'.
 removeMethodButton := self newButton.
	 removeMethodButton label: 'remove current Method'.
	saveSettingButton := self newButton.
	saveSettingButton label: 'save'.
	
	removeSettingButton := self newButton.
	removeSettingButton label: 'remove setting'.

    self setupMethodDropList.
    self setupSettingsDropList.
    self setupSettingsTree.
    self setupBeforePrettyPrinterCodePresenter.
   "self setupAfterPrettyPrinterCodePresenter."
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
        add: notFormatedTextPresenter;
        add: formatedTextPresenter
]

{ #category : #initialization }
EFSettingPreviewer2 >> initializeWindow: aWindowPresenter [
    aWindowPresenter title: 'Blue Ink Setting Previewer'.
    aWindowPresenter initialExtent: 1000 @ 700
]

{ #category : #'system-Announcement' }
EFSettingPreviewer2 >> methodAdded: aMethodAdded [
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
EFSettingPreviewer2 >> methodDropList [
	^ methodDropList
]

{ #category : #'system-Announcement' }
EFSettingPreviewer2 >> methodModified: aModifiedMethod [
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
EFSettingPreviewer2 >> methodProvider [
	^ methodProvider
]

{ #category : #api }
EFSettingPreviewer2 >> methodProvider: aMethodProvider [
	methodProvider := aMethodProvider.
	methodDropList items: (methodProvider methods select: [ :method | 
			 method category = 'methods' ]) asOrderedCollection.
	settingsDropList items:
		(methodProvider class methods select: [ :method | 
			 method category = 'settings' ]) asOrderedCollection.
	methodDropList selectIndex: 1
]

{ #category : #'system-Announcement' }
EFSettingPreviewer2 >> methodRemoved: aMethodRemoved [
	(aMethodRemoved methodOrigin = methodProvider and: [ 
		 isSettingModification not ]) ifFalse: [ ^ self ].
	methodDropList selectIndex: 1.
	methodDropList items: (methodDropList listItems
			 removeAllSuchThat: [ :method | 
				 method selector = aMethodRemoved method selector ];
			 yourself)
]

{ #category : #accessing }
EFSettingPreviewer2 >> notFormatedTextPresenter [
	^ notFormatedTextPresenter
]

{ #category : #'system-Announcement' }
EFSettingPreviewer2 >> onWindowClosed [
	self unregisterAnnouncement.
]

{ #category : #accessing }
EFSettingPreviewer2 >> removeMethodButton [
	^ removeMethodButton
]

{ #category : #accessing }
EFSettingPreviewer2 >> removeSettingButton [
	^ removeSettingButton
]

{ #category : #accessing }
EFSettingPreviewer2 >> saveMethodButton [
	^ saveMethodButton
]

{ #category : #accessing }
EFSettingPreviewer2 >> saveSettingButton [
	^ saveSettingButton
]

{ #category : #accessing }
EFSettingPreviewer2 >> settingsDropList [
	^ settingsDropList
]

{ #category : #accessing }
EFSettingPreviewer2 >> settingsTree [
	^ settingsTree
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> setupAfterPrettyPrinterCodePresenter [
	formatedTextPresenter withSyntaxHighlight.
	formatedTextPresenter text: self formatPrettyPrint
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> setupBeforePrettyPrinterCodePresenter [
	notFormatedTextPresenter
		withSyntaxHighlight;
		autoAccept: true.
	"notFormatedTextPresenter
		text: methodDropList selectedItem sourceCode"
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> setupMethodDropList [
	methodDropList
		items:
			(methodProvider methods select: [ :each | 
					 each category = 'methods' ]) asOrderedCollection;
		display: [ :item | item selector ]
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> setupSettingsDropList [
	settingsDropList
		items: ((methodProvider methods , methodProvider class methods)
				 asOrderedCollection select: [ :each | each category = 'settings' ]);
		display: [ :each | each selector ].
	"formaterPrettyPrinter installNewContext:
		(STON fromString: (settingsDropList selectedItem
				  valueWithReceiver: settingsDropList selectedItem origin
				  arguments: #(  )))"
]

{ #category : #'initialization widgets' }
EFSettingPreviewer2 >> setupSettingsTree [
	settingsTree
		roots: self getSystemsettingsAsParentName;
		addColumn: (SpStringTableColumn evaluated: [ :eachItem | eachItem item label ]);
		addColumn:
			(SpImageTableColumn
				evaluated: [ :eachItem | eachItem settingDeclaration inputWidget ])
]

{ #category : #'system-Announcement' }
EFSettingPreviewer2 >> unregisterAnnouncement [
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #'system-Announcement' }
EFSettingPreviewer2 >> whenASettingChanged [
	formatedTextPresenter text: self formatPrettyPrint 
]

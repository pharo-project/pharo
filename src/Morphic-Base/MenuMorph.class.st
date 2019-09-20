"
Instance variables:
	defaultTarget 	<Object>				The default target for creating menu items
	selectedItem		<MenuItemMorph> 	The currently selected item in the receiver
	stayUp 			<Boolean>			True if the receiver should stay up after clicks
	popUpOwner 	<MenuItemMorph>	The menu item that automatically invoked the receiver, if any.
	activeSubMenu 	<MenuMorph>		The currently active submenu.
"
Class {
	#name : #MenuMorph,
	#superclass : #AlignmentMorph,
	#instVars : [
		'defaultTarget',
		'selectedItem',
		'stayUp',
		'popUpOwner',
		'activeSubMenu',
		'titleMorph'
	],
	#classVars : [
		'CloseBoxImage',
		'PushPinImage'
	],
	#category : #'Morphic-Base-Menus'
}

{ #category : #'private - utilities' }
MenuMorph class >> chooseFrom: aList lines: linesArray title: queryString [
	"Choose an item from the given list. Answer the index of the selected item. Do not invoke this method - it is here to help in case of mini system, better use UIManager default"
	
	
	"MenuMorph
		chooseFrom: #('Hello' 'Pharoers' 'Here' 'We' 'Go')
		lines: #(2 4)
		title: 'What''s up?'"
		
	| menu result |
	(ProvideAnswerNotification signal: queryString) ifNotNil:[:answer | 
		1 to: aList size do:[:i| (aList at: i) = answer ifTrue:[^i]].
		^0].
	result := 0.
	menu := self new.
	menu addTitle: queryString.
	1 to: aList size do:[:i| 
		menu add: (aList at: i) asString target: [:v| result := v] selector: #value: argument: i.
		(linesArray includes: i) ifTrue: [menu addLine]].
	
	menu invokeAt: ActiveHand position in: self currentWorld allowKeyboard: true.
	^result
]

{ #category : #'private - utilities' }
MenuMorph class >> chooseFrom: aList values: valueList lines: linesArray title: queryString [
	"Choose an item from the given list. Answer the index of the selected item.  Do not invoke this method - it is here to help in case of mini system, better use UIManager default"
	
	"MenuMorph
		chooseFrom: #('Hello' 'Pharoers' 'Here' 'We' 'Go')
		values: #('Ph' 'Pha' 'Pharo' 'Yeah' 'YeahYeah')
		lines: #(2 4)
		title: 'What''s up?'"
		
	| menu result |
	(ProvideAnswerNotification signal: queryString) ifNotNil:[:answer | 
		1 to: aList size do:[:i| (aList at: i) = answer ifTrue:[^answer]].
		^nil].
	result := nil.
	menu := self new.
	menu addTitle: queryString.
	1 to: aList size do:[:i| 
		menu add: (aList at: i) asString target: [:v| result := v] selector: #value: argument: (valueList at: i).
		(linesArray includes: i) ifTrue:[menu addLine]].

	menu invokeAt: ActiveHand position in: self currentWorld allowKeyboard: true.
	^result
]

{ #category : #images }
MenuMorph class >> closeBoxImage [
	"Supplied here because we don't necessarily have ComicBold"

	^ CloseBoxImage ifNil: [CloseBoxImage := SystemWindow closeBoxImage]
]

{ #category : #utilities }
MenuMorph class >> confirm: queryString [
	"Put up a yes/no menu with caption queryString. Answer true if the response is yes, false if no. This is a modal question--the user must respond yes or no. Do not invoke this method - it is here to help in case of mini system, better use
	UIManager default confirm: 'Is is the question?' "

	^ self confirm: queryString trueChoice: 'Yes' translated falseChoice: 'No' translated
]

{ #category : #'private - utilities' }
MenuMorph class >> confirm: queryString orCancel: cancelBlock [ 
	"Put up a yes/no/cancel menu with caption aString. Answer true if  the response is yes, false if no. If cancel is chosen, evaluate cancelBlock. This is a modal question--the user must respond yes or no.  Do not invoke this method - it is here to help in case of mini system, better use UIManager default"
	"MenuMorph confirm: 'Reboot universe now' orCancel:[^'Nevermind'] "
	
	| choice |
	choice := self chooseFrom: {'Yes' translated. 'No' translated. 'Cancel' translated}
		lines: #()
		title: queryString.
	choice = 1 ifTrue: [^ true].
	choice = 2 ifTrue: [^ false].
	^ cancelBlock value
]

{ #category : #'private - utilities' }
MenuMorph class >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice [
	"Put up a yes/no menu with caption queryString. The actual wording for the two choices will be as provided in the trueChoice and falseChoice parameters. Answer true if the response is the true-choice,  false if it's the false-choice. This is a modal question -- the user must respond one way or the other.  Do not invoke this method - it is here to help in case of mini system, better use UIManager default"

	"MenuMorph 
		confirm: 'Are you sure?'  
		trueChoice: 'yes, I''m '  
		falseChoice: 'no, I just thought'"

	| menu aBlock result |
	(ProvideAnswerNotification signal: queryString)
		ifNotNil: [ :answer | ^ trueChoice = answer ].
	aBlock := [ :v | result := v ].
	menu := self new.
	menu addTitle: queryString icon: (self iconNamed: #confirmIcon).
	menu
		add: trueChoice
		target: aBlock
		selector: #value:
		argument: true.
	menu
		add: falseChoice
		target: aBlock
		selector: #value:
		argument: false.
	[ menu
		invokeAt: ActiveHand position
		in: self currentWorld
		allowKeyboard: true.
	result isNil ] whileTrue.
	^ result
]

{ #category : #'instance creation' }
MenuMorph class >> entitled: aString [
	"Answer a new instance of me with the given title."

	^ self new addTitle: aString

]

{ #category : #examples }
MenuMorph class >> example [
	"MenuMorph example openInHand"
	<example>
	<sampleInstance>
	
	| menu |
	menu := MenuMorph new.
	menu buildTitle: [ :menuTitle | menuTitle
		onlyCloseAndPinable
	].
	menu add: 'apples' selector: #apples.
	menu add: 'oranges' selector: #oranges.
	menu addLine.
	"menu addLine. " "extra lines ignored" 
	menu add: 'peaches' selector: #peaches.
	menu addLine.
	menu add: 'pears' selector: #pears.
	menu addLine.
	^ menu popUpInWorld

]

{ #category : #'instance creation' }
MenuMorph class >> fromList: anArray [ 
	"Construct a menu from an array of descriptions. See #addList comment"

	^ self new addList: anArray.

]

{ #category : #'instance creation' }
MenuMorph class >> initialize [
	"MenuMorph initialize"
	PushPinImage := nil
]

{ #category : #images }
MenuMorph class >> pushPinImage [
	"Answer the push-pin image, creating and caching
	it at this time if it is absent"
	^ PushPinImage
				ifNil: [PushPinImage := Form
								extent: 16 @ 16
								depth: 32
								fromArray: #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4289374634 4280690213 4282467139 4281811785 4286217083 0 0 0 0 0 0 0 0 0 0 0 4279900698 4289771447 4283150819 4278686942 4278602609 4281216819 4292862175 0 0 0 0 0 0 0 0 4292598747 4278519045 4291812321 4278425828 4278229220 4278360034 4278533726 4281676595 0 0 0 0 0 0 0 0 4293059298 4278781959 4289902007 4280591330 4278294757 4278359779 4278618315 4278454800 4287730065 0 0 0 0 4293717228 4289835441 4291743438 4288782753 4278782730 4283980117 4287155693 4278294756 4278360036 4278425831 4278725183 4281348657 0 0 0 4293190884 4281413937 4281677109 4278387459 4278584069 4278457889 4278717198 4285372595 4278753764 4278359781 4278556389 4278468957 4278650887 0 0 0 4286019447 4284243036 4283914071 4278781702 4285033581 4279932888 4278683597 4278490589 4278490848 4278620633 4278621404 4278591793 4279242768 0 0 0 4283519312 4285295466 4290165174 4290164405 4294638071 4282232039 4278491363 4278620380 4278723896 4278519564 4278389263 4278387459 4285427310 0 0 0 4285887863 4280431419 4286696174 4290634484 4286170860 4278818529 4278619863 4278661191 4278913293 4285493359 4284177243 4288585374 4294177779 0 0 0 4291480781 4278322439 4278614713 4278490852 4278622435 4278613940 4278458404 4278321667 4278518531 4288914340 0 0 0 0 0 0 0 4281018922 4278464064 4278359263 4278491102 4278724669 4278518276 4278387461 4278321666 4282532418 0 0 0 0 0 0 4292730333 4279045132 4278584327 4278665827 4278489307 4278621404 4278480807 4278595138 4278453252 4281677109 0 0 0 0 0 0 4284900966 4278848010 4283650898 4278781962 4278523682 4278726730 4278592304 4278454027 4278519045 4287861651 0 0 0 0 0 0 4280887593 4290493371 0 4290822079 4284308832 4280163615 4279439633 4281611320 4288322202 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 )
								offset: 0 @ 0]
]

{ #category : #events }
MenuMorph >> activate: evt [
	"Receiver should be activated; e.g., so that control passes correctly."
	evt hand newMouseFocus: self
]

{ #category : #events }
MenuMorph >> activateFromKeyboard: evt [
	"Receiver should be activated; e.g., so that control passes correctly."
	evt hand newMouseFocus: self.
	self activate: evt.
	self takeKeyboardFocus.
	self moveSelectionDown: 1 event: evt
]

{ #category : #control }
MenuMorph >> activeSubmenu: aSubmenu [ 
	activeSubMenu
		ifNotNil: [activeSubMenu delete].
	activeSubMenu := aSubmenu.
	
]

{ #category : #construction }
MenuMorph >> add: aLabelString icon: aForm subMenu: aMenuMorph [
	"Append the given submenu with the given label."

	self addToggle: aLabelString target: nil selector: nil.
	self lastItem
		icon: aForm;
		subMenu: aMenuMorph.
	^ self
]

{ #category : #construction }
MenuMorph >> add: aLabelString selector: aSymbol [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	self add: aLabelString
		target: defaultTarget
		selector: aSymbol
		argumentList: EmptyArray.

]

{ #category : #construction }
MenuMorph >> add: aLabelString selector: aSymbol argument: arg [

	^ self add: aLabelString
		target: defaultTarget
		selector: aSymbol
		argumentList: (Array with: arg)

]

{ #category : #construction }
MenuMorph >> add: aLabelString subMenu: aMenuMorph [ 
	"Append the given submenu with the given label."
	self
		add: aLabelString
		icon: nil
		subMenu: aMenuMorph
]

{ #category : #construction }
MenuMorph >> add: aLabelString target: anObject selector: aSymbol [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	^ self add: aLabelString
		target: anObject
		selector: aSymbol
		argumentList: EmptyArray.

]

{ #category : #construction }
MenuMorph >> add: aLabelString target: target selector: aSymbol argument: arg [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	^ self add: aLabelString
		target: target
		selector: aSymbol
		argumentList: (Array with: arg)

]

{ #category : #construction }
MenuMorph >> add: aLabelString target: target selector: aSymbol argumentList: argList [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument."

	^ self
		addToggle: aLabelString
		target: target
		selector: aSymbol
		getStateSelector: nil
		enablementSelector: nil
		argumentList: argList
]

{ #category : #construction }
MenuMorph >> addAllFrom: aMenuMorph [
	"This is a fast add..."
	
	aMenuMorph submorphsDo: [ :m | self addMorphBack: m ]
]

{ #category : #construction }
MenuMorph >> addAllFromPragma: aString target: anObject [
	self addAllFrom: (PragmaMenuBuilder 
		pragmaKeyword: aString
		model: anObject)
		menu 
]

{ #category : #construction }
MenuMorph >> addBlankIconsIfNecessary: anIcon [ 
	"If any of my items have an icon, ensure that all do by using  
	anIcon for those that don't"
	self items
		reject: [:each | each hasIconOrMarker]
		thenDo: [:each | each icon: anIcon]
]

{ #category : #menu }
MenuMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu add: 'add title...' selector: #addTitle.
	aCustomMenu add: 'set target...' selector: #setTarget:.
	defaultTarget ifNotNil: [
		aCustomMenu add: 'add item...' selector: #addItem].
	aCustomMenu add: 'add line' selector: #addLine.
	(self items anySatisfy: [ :any | any hasSubMenu ]) ifTrue: [
		 aCustomMenu add: 'detach submenu' selector: #detachSubMenu: ].
]

{ #category : #actions }
MenuMorph >> addIfNeededTitle: aTitle andIcon: anIcon [

	(aTitle notNil or: [ anIcon notNil ])
		ifTrue: [ self addTitle: aTitle icon: anIcon ]
]

{ #category : #'construction-UIManager' }
MenuMorph >> addItem [

	| string sel |
	string := UIManager default request: 'Label for new item?'.
	string isEmpty ifTrue: [^ self].
	sel := UIManager default request: 'Selector?'.
	sel isEmpty ifFalse: [sel := sel asSymbol].
	self add: string selector: sel.

]

{ #category : #construction }
MenuMorph >> addLine [
	"Append a divider line to this menu. Suppress duplicate lines."
	(self hasItems and: [ self lastSubmorph isMenuLineMorph not ])
		ifTrue: [ self addMorphBack: MenuLineMorph new ].
		
		
		
	"(self hasItems)
		ifTrue: [ self addMorphBack: MenuLineMorph new .self addMorphBack: MenuLineMorph new]."
		
	"Looks ok when allowing more than one line after another line...."
]

{ #category : #construction }
MenuMorph >> addList: aList [
	"Add the given items to this menu, where each item is a pair (<string> <actionSelector>)..  If an element of the list is simply the symobl $-, add a line to the receiver.  The optional third element of each entry, if present, provides balloon help. The optional fourth element provide the icon selector"

	aList
		do: [ :tuple | 
			tuple == #-
				ifTrue: [ self addLine ]
				ifFalse: [ 
					self add: tuple first capitalized translated selector: tuple second.
					(tuple size > 2 and: [ tuple third notNil ])
						ifTrue: [ self balloonTextForLastItem: tuple third translated ].
					(tuple size > 3 and: [ tuple fourth notNil ])
						ifTrue: [ self lastItem icon: (self iconNamed: tuple fourth) ] ] ]
]

{ #category : #construction }
MenuMorph >> addMenuItem: anItem [
	"Like addMorphBack: but returns the argument. The return value is used by the
	#addToggle:target:selector: and #addTarget:selector: families of messages."

	self addMorphBack: anItem.
	^ anItem

]

{ #category : #'construction-UIManager' }
MenuMorph >> addTitle [

	| string |
	string := UIManager default request: 'Title for this menu?'.
	string isEmptyOrNil ifTrue: [^ self].
	self addTitle: string.

]

{ #category : #'construction-title' }
MenuMorph >> addTitle: aString [
	"Add a title line at the top of this menu."
	
	 self buildTitle: [ :tm | tm title: aString ].
		
	
]

{ #category : #'construction-title' }
MenuMorph >> addTitle: aString icon: aForm [
	"Add a title line at the top of this menu."
	
	aForm ifNil: [ ^self addTitle: aString ].
	
	self buildTitle: [ :tm | tm 
		title: aString;
		icon: aForm
	].
]

{ #category : #construction }
MenuMorph >> addToggle: aString selector: aSymbol [
	"Append a menu item with the given label. If the item is selected,
	it will send the given selector to the default target object."

	self addToggle: aString
		target: defaultTarget
		selector: aSymbol
		getStateSelector: nil
		argumentList: EmptyArray
]

{ #category : #construction }
MenuMorph >> addToggle: aString target: anObject selector: aSymbol [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	self addToggle: aString
		target: anObject
		selector: aSymbol
		getStateSelector: nil
		argumentList: EmptyArray
]

{ #category : #construction }
MenuMorph >> addToggle: aString target: anObject selector: aSymbol getStateSelector: stateSymbol [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	self addToggle: aString
		target: anObject
		selector: aSymbol
		getStateSelector: stateSymbol
		argumentList: EmptyArray
]

{ #category : #construction }
MenuMorph >> addToggle: aString target: anObject selector: aSymbol getStateSelector: stateSymbol argumentList: argList [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	self addToggle: aString
		target: anObject
		selector: aSymbol
		getStateSelector: stateSymbol
		enablementSelector: nil
		argumentList: argList
]

{ #category : #construction }
MenuMorph >> addToggle: aString target: anObject selector: aSymbol getStateSelector: stateSymbol enablementSelector: enableSymbol [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	self addToggle: aString
		target: anObject
		selector: aSymbol
		getStateSelector: stateSymbol
		enablementSelector: enableSymbol
		argumentList: EmptyArray
]

{ #category : #construction }
MenuMorph >> addToggle: aString target: anObject selector: aSymbol getStateSelector: stateSymbol enablementSelector: enableSymbol argumentList: argList [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	|item|
	item := ToggleMenuItemMorph new
		contents: aString;
		target: anObject;
		selector: aSymbol;
		arguments: argList;
		getStateSelector: stateSymbol;
		enablementSelector: enableSymbol.
	^ self addMenuItem: item

]

{ #category : #construction }
MenuMorph >> addUpdating: aWordingSelector selector: aSymbol [

	self addUpdating: aWordingSelector target: defaultTarget selector: aSymbol argumentList: EmptyArray

]

{ #category : #construction }
MenuMorph >> addUpdating: aWordingSelector target: aTarget selector: aSymbol [

	self addUpdating: aWordingSelector target: aTarget selector: aSymbol argumentList: EmptyArray

]

{ #category : #construction }
MenuMorph >> addUpdating: wordingSelector target: target selector: aSymbol argumentList: argList [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the initial wording of the menu item is obtained by sending the wordingSelector to the target. If the wording prefixed with <yes> or <no>, the on/off state of the menu item will reflect it."
 
	|aString str|
	aString := (MessageSend receiver: target selector: wordingSelector) valueWithEnoughArguments: argList.
	self flag: #pharoFixMe.
	(aString isKindOf: Association)
	     ifTrue: [aString := aString value]
		ifFalse: [
				str := aString readStream.
				(str skipTo: $>) ifTrue: [aString := str upToEnd]].
	self
		addToggle: aString
		target: target
		selector: aSymbol
		getStateSelector: wordingSelector
		enablementSelector: nil
		argumentList: argList

]

{ #category : #construction }
MenuMorph >> addWithLabel: aLabelString enablement: anEnablementSelector action: aSymbol [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is constant, and the optional enablementSelector determines whether or not the item should be enabled."

	self flag: #pharoToDeprecate.
	self
		addToggle: aLabelString
		target: defaultTarget
		selector: aSymbol
		getStateSelector: nil
		enablementSelector: anEnablementSelector
		argumentList: EmptyArray
]

{ #category : #actions }
MenuMorph >> adoptPaneColor: paneColor [
	"Change our color."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self color: paneColor
]

{ #category : #construction }
MenuMorph >> balloonTextForLastItem: balloonText [
	submorphs last setBalloonText: balloonText
]

{ #category : #'construction-title' }
MenuMorph >> buildTitle: aBlock [ 
	
"
	Example usage:

	menu buildTitle: [ :titleMorph | titleMorph
		title: 'aCoolTitle';
		icon: self theme icons alertIcon;
		withCloseBox;
		withPinBox;
		color: Color purple		
	].
"
	
	titleMorph ifNil: [ self createTitleMorph ].
	
	aBlock value: titleMorph.
]

{ #category : #accessing }
MenuMorph >> color: aColor [
	"Set the receiver's color. Remember the base color in the case of a gradient background."

	super color: aColor.
	self setProperty: #basicColor toValue: aColor
]

{ #category : #accessing }
MenuMorph >> commandKeyHandler [
	"Answer the receiver's commandKeyHandler"

	^ self valueOfProperty: #commandKeyHandler ifAbsent: [nil]
]

{ #category : #accessing }
MenuMorph >> commandKeyHandler: anObject [
	"Set the receiver's commandKeyHandler.  Whatever you set here needs to be prepared to respond to the message #commandKeyTypedIntoMenu: "

	self setProperty: #commandKeyHandler toValue: anObject
]

{ #category : #'construction-title' }
MenuMorph >> createTitleMorph [

	titleMorph ifNotNil: [ titleMorph delete ].
	titleMorph := MenuTitleMorph new.
	
	titleMorph 
	on: #mouseDown send: #mouseDownInTitle: to: self.
	"maxCellSize:World width /2;"
	
	self addMorphFront: titleMorph.
]

{ #category : #accessing }
MenuMorph >> defaultTarget [
	^defaultTarget
]

{ #category : #accessing }
MenuMorph >> defaultTarget: anObject [
	"Set the default target for adding menu items."

	defaultTarget := anObject.

]

{ #category : #initialization }
MenuMorph >> delete [

	activeSubMenu ifNotNil:[activeSubMenu delete].
	^super delete
]

{ #category : #actions }
MenuMorph >> deleteIfPopUp [
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [self topRendererOrSelf delete].
	(popUpOwner notNil and: [popUpOwner isMenuItemMorph]) ifTrue: [
		popUpOwner isSelected: false.
		(popUpOwner owner isMenuMorph)
			ifTrue: [popUpOwner owner deleteIfPopUp]].

]

{ #category : #control }
MenuMorph >> deleteIfPopUp: evt [
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."
	stayUp ifFalse: [self topRendererOrSelf delete].
	(popUpOwner notNil) ifTrue: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt].
	evt ifNotNil:[evt hand releaseMouseFocus: self].
]

{ #category : #control }
MenuMorph >> deselectAndFocusOutermenuOn: anEvent [
	"deselect and return focus to outer menu"

	self selectItem: nil event: anEvent.
	anEvent hand newMouseFocus: popUpOwner owner.
	^ anEvent hand newKeyboardFocus: popUpOwner owner
]

{ #category : #menu }
MenuMorph >> detachSubMenu: evt [
	| possibleTargets item subMenu index |
	possibleTargets := self items select:[ :any | any hasSubMenu].
	possibleTargets isEmpty ifTrue: [ ^self ].
	
	index := UIManager default  
			chooseFrom: (possibleTargets collect:[:t| t contents asString]) 
			title: 'Which menu?' translated.
	index = 0 ifTrue:[^self].
	
	item := possibleTargets at: index.
	subMenu := item subMenu.
	subMenu ifNotNil: [
		item subMenu: nil.
		item delete.
		subMenu stayUp: true.
		subMenu popUpOwner: nil.
		subMenu addTitle: item contents.
		evt hand attachMorph: subMenu
	].

]

{ #category : #control }
MenuMorph >> displayAt: aPoint during: aBlock [
	"Add this menu to the Morphic world during the execution of the given block."
	self currentWorld addMorph: self centeredNear: aPoint.
	self world displayWorld.  "show myself"
	aBlock value.
	self delete
]

{ #category : #'keyboard control' }
MenuMorph >> displayFiltered: evt [
	| matchStr matches feedbackMorph |

	matchStr := self valueOfProperty: #matchString.
	matches := self menuItems select: [:m | | isMatch | 
		isMatch := matchStr isEmpty or: [
			m contents includesSubstring: matchStr caseSensitive: false
		].
		m isEnabled: isMatch.
		isMatch
	].
	feedbackMorph := self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph := 
			TextMorph new 
				autoFit: true;
				color: Color darkGray.
		self
			addLine;
			addMorphBack: feedbackMorph lock.
		self setProperty: #feedbackMorph toValue: feedbackMorph.
		self fullBounds.  "Lay out for submorph adjacency"].
	feedbackMorph contents: '<', matchStr, '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self lastSubmorph delete.
		self removeProperty: #feedbackMorph
	].
	    " This method is invoked with evt = nil from MenuMorph >> removeMatchString. The current implementation can't select an item without an event. (Yet :D)"
       (evt notNil and: [ matches size >= 1 ]) 
		ifTrue: [self selectItem: matches first event: evt]

]

{ #category : #menu }
MenuMorph >> doButtonAction [
	"Do the receiver's inherent button action.  Makes sense for the kind of MenuMorph that is a wrapper for a single menu-item -- pass it on the the item"

	self hasItems ifTrue: [ self menuItems first doButtonAction ]
]

{ #category : #drawing }
MenuMorph >> drawOn: aCanvas [ 
	"Draw the menu. Add keyboard-focus feedback if appropriate"

	super drawOn: aCanvas.

	(ActiveHand notNil
			and: [ActiveHand keyboardFocus == self
					and: [self rootMenu hasProperty: #hasUsedKeyboard]])
		ifTrue: [
			aCanvas
				frameAndFillRectangle: self innerBounds
				fillColor: Color transparent
				borderWidth: self theme menuBorderWidth
				borderColor: self theme menuKeyboardFocusColor
		].

]

{ #category : #accessing }
MenuMorph >> embeddable [
	^self valueOfProperty: #embeddable ifAbsent: [ false ]. 

]

{ #category : #accessing }
MenuMorph >> embeddable: aBoolean [
	self setProperty: #embeddable toValue: aBoolean
]

{ #category : #'keyboard control' }
MenuMorph >> filterListWith: char [ 
	| matchString |
	matchString := self valueOfProperty: #matchString ifAbsentPut: [String new].
	matchString := char = Character backspace 
				ifTrue: 
					[matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: char].
	self setProperty: #matchString toValue: matchString
]

{ #category : #filtering }
MenuMorph >> getFiltering: matchString [
	^ self valueOfProperty: #matchString ifAbsentPut: [ String new ]
]

{ #category : #events }
MenuMorph >> handleFocusEvent: evt [
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	self processEvent: evt.

	"Need to handle keyboard input if we have the focus."
	evt isKeyboard ifTrue: [^ self handleEvent: evt].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(evt isMouseOver or:[evt isMouse not]) ifTrue:[^self].
	"What remains are mouse buttons and moves"
	evt isMove ifFalse:[^self handleEvent: evt]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: evt) ifTrue:[^self]].
	"If we aren't under the hand anymore, transfer to the owner"
   (self fullContainsPoint: evt position) ifFalse:[
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: evt) ifTrue:[^self]]]
]

{ #category : #'keyboard control' }
MenuMorph >> handlesKeyboard: evt [
	"Answer whether the receiver handles the keystroke represented by the event"

	^ evt anyModifierKeyPressed not or: [evt commandKeyPressed and: [self commandKeyHandler notNil]]
]

{ #category : #events }
MenuMorph >> handlesMouseDown: evt [
	^true
]

{ #category : #testing }
MenuMorph >> hasItems [
	"Answer if the receiver has menu items"
	^ submorphs anySatisfy: [:each | each isMenuItemMorph ]

]

{ #category : #testing }
MenuMorph >> hasSubMenu: aMenuMorph [

	^submorphs anySatisfy: [ :m | m isMenuItemMorph and: (m hasSubMenu: aMenuMorph) ]

]

{ #category : #initialization }
MenuMorph >> initialize [
	super initialize.

	self setDefaultParameters.
	self listDirection: #topToBottom.
	self hResizing: #shrinkWrap.
	self vResizing: #shrinkWrap.
	stayUp := false.
	self setProperty: #morphicLayerNumber toValue: self morphicLayerNumber.
	self theme currentSettings preferRoundCorner ifTrue: [ self useRoundedCorners ]
]

{ #category : #keymapping }
MenuMorph >> initializeShortcuts: aKMDispatcher [
	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #MorphFocusNavigation
]

{ #category : #invoking }
MenuMorph >> invokeAt: aPoint in: aWorld [
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."

	^ self invokeAt: aPoint in: aWorld allowKeyboard: self  menuKeyboardControl
]

{ #category : #invoking }
MenuMorph >> invokeAt: aPoint in: aWorld allowKeyboard: aBoolean [
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	| w originalFocusHolder |
	originalFocusHolder := aWorld primaryHand keyboardFocus.
	self popUpAt: aPoint forHand: aWorld primaryHand in: aWorld allowKeyboard: aBoolean.
	w := aWorld outermostWorldMorph. "containing hand"
	[self isInWorld] whileTrue: [w doOneSubCycle].
	self delete.
	self restoreFocus: originalFocusHolder in: aWorld.
	^ selectedItem ifNotNil: [selectedItem target]

]

{ #category : #private }
MenuMorph >> invokeMetaMenu: evt [
	stayUp ifFalse:[^self]. "Don't allow this"
	^super invokeMetaMenu: evt
]

{ #category : #'modal control' }
MenuMorph >> invokeModal [
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: self menuKeyboardControl

	"Example:
	| menu sub entry |
	menu := MenuMorph new.
	1 to: 3 do: [:i |
		entry := 'Line', i printString.
		sub := MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				selector: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
"


]

{ #category : #'modal control' }
MenuMorph >> invokeModal: allowKeyboardControl [
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu"

	^ self invokeModalAt: ActiveHand position in: self currentWorld allowKeyboard: allowKeyboardControl
]

{ #category : #'modal control' }
MenuMorph >> invokeModalAt: aPoint in: aWorld allowKeyboard: aBoolean [
	"Invoke this menu and don't return until the user has chosen a value.
	See senders of this method for finding out how to use modal menu morphs."
	| w originalFocusHolder |
	originalFocusHolder := aWorld primaryHand keyboardFocus.
	self popUpAt: aPoint forHand: aWorld primaryHand in: aWorld allowKeyboard: aBoolean.
	self isModalInvokationDone: false.
	w := aWorld outermostWorldMorph. "containing hand"
	[self isInWorld and: [ self isModalInvokationDone not ] ] whileTrue: [w doOneSubCycle].
	self delete.
	self restoreFocus: originalFocusHolder in: aWorld.
	^ self modalSelection
]

{ #category : #testing }
MenuMorph >> isMenuMorph [
	^ true
]

{ #category : #'modal control' }
MenuMorph >> isModalInvokationDone [
	^self valueOfProperty: #isModalInvokationDone ifAbsent:[false]
]

{ #category : #'modal control' }
MenuMorph >> isModalInvokationDone: aBool [
	self setProperty: #isModalInvokationDone toValue: aBool

]

{ #category : #accessing }
MenuMorph >> items [
	"Returns menu items filtering out menu lines"
	^self menuItems

]

{ #category : #'dropping/grabbing' }
MenuMorph >> justDroppedInto: aMorph event: evt [
	| halo |
	super justDroppedInto: aMorph event: evt.
	halo := evt hand halo.
	(halo notNil and:[halo target hasOwner: self]) ifTrue:[
		"Grabbed single menu item"
		self addHalo: evt.
	].
	stayUp ifFalse:[evt hand newMouseFocus: self].
]

{ #category : #events }
MenuMorph >> keyStroke: evt [
	"Handle keboard item matching."

	| matchString char asc selectable help |
	help := self theme builder
		newBalloonHelp: 'Enter text to\narrow selection down\to matching items ' withCRs
		for: self
		corner: #topLeft.
	help popUpForHand: self activeHand.
	(self rootMenu hasProperty: #hasUsedKeyboard)
		ifFalse: [ self rootMenu setProperty: #hasUsedKeyboard toValue: true.
			self changed ].
	(evt commandKeyPressed and: [ self commandKeyHandler notNil ])
		ifTrue: [ self commandKeyHandler commandKeyTypedIntoMenu: evt.
			^ self deleteIfPopUp: evt ].
	char := evt keyCharacter.
	asc := char asciiValue.
	char = Character cr
		ifTrue: [ selectedItem
				ifNotNil: [ selectedItem hasSubMenu
						ifTrue: [ evt hand newMouseFocus: selectedItem subMenu.
							^ evt hand newKeyboardFocus: selectedItem subMenu ]
						ifFalse: [ "self delete." ^ selectedItem invokeWithEvent: evt ] ].
			(selectable := self items) size = 1
				ifTrue: [ ^ selectable first invokeWithEvent: evt ].
			^ self ].
	asc = 27
		ifTrue: [ "escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty
						ifFalse: [ "If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil event: evt.
							^ self displayFiltered: evt ] ].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [ ^ self delete ].
			"If a sub-menu, then deselect, and return focus to outer menu"
			^ self deselectAndFocusOutermenuOn: evt ].

	"Left arrow key - If we are in a submenu, then we remove myself (i.e., the current morph) and move the focus to the owner popup"
	asc = 28 ifTrue: [ ^ self leftArrowStroked: evt ].

	"Right arrow key - If the selected menu item has a submenu, then we move the focus to the submenu "
	asc = 29 ifTrue: [ (self rightArrowStroked: evt) ifTrue: [ ^ self ] ].
	asc = 30
		ifTrue: [ ^ self moveSelectionDown: -1 event: evt ].	"up arrow key"
	asc = 31
		ifTrue: [ ^ self moveSelectionDown: 1 event: evt ].	"down arrow key"
	asc = 11
		ifTrue: [ ^ self moveSelectionDown: -5 event: evt ].	"page up key"
	asc = 12
		ifTrue: [ ^ self moveSelectionDown: 5 event: evt ].	"page down key"

	"If we reach this point, it means that we are editing the filter associated to each menu. "
	"In case ther eis no filter associated to the menu, we simply create one"
	matchString := self
		valueOfProperty: #matchString
		ifAbsentPut: [ String new ].

	"If we press the backspace, then we simply remove the last character from matchString"
	(char = Character backspace and: [ matchString notEmpty ])
		ifTrue: [ matchString := matchString allButLast.
			self recordFiltering: matchString.
			self displayFiltered: evt ].

	"No need to go further if the character is not alphanumeric, i.e., not useful for filtering"
	char isAlphaNumeric
		ifFalse: [ ^ self ].
	matchString := matchString , char asString.
	self recordFiltering: matchString.
	self displayFiltered: evt
]

{ #category : #'keyboard control' }
MenuMorph >> keyboardFocusChange: aBoolean [
	"Notify change due to green border for keyboard focus"
	super keyboardFocusChange: aBoolean.
	self changed
]

{ #category : #accessing }
MenuMorph >> lastItem [

	submorphs reverseDo: [ :each | (each isMenuItemMorph) ifTrue: [ ^each ] ].
	^submorphs last
]

{ #category : #accessing }
MenuMorph >> lastSelection [
	"Return the label of the last selected item or nil."

	^selectedItem ifNotNil: [selectedItem selector].
]

{ #category : #control }
MenuMorph >> layoutItems [
	"decorate aMenu with icons"

	| maxIconWidth |
	maxIconWidth := 0.

	self items do: [:item | 
		item icon ifNotNil: [maxIconWidth := maxIconWidth max: item icon width].
		item hasSubMenu ifTrue: [item subMenu layoutItems]].
	maxIconWidth isZero 
		ifFalse: [self addBlankIconsIfNecessary: (Smalltalk ui icons blankIconOfWidth: maxIconWidth)].

]

{ #category : #events }
MenuMorph >> leftArrowStroked: evt [
	"If a stand-alone menu, do nothing"
	popUpOwner ifNil: [ ^ self ].
	"If a sub-menu, then deselect, and return focus to outer menu"
	^ self deselectAndFocusOutermenuOn: evt
]

{ #category : #accessing }
MenuMorph >> menuItems [
	^submorphs select: [ :m | m isMenuItemMorph ]

]

{ #category : #'modal control' }
MenuMorph >> modalSelection [
	^self valueOfProperty: #modalSelection ifAbsent:[nil]
]

{ #category : #'modal control' }
MenuMorph >> modalSelection: anObject [
	self setProperty: #modalSelection toValue: anObject.
	self isModalInvokationDone: true
]

{ #category : #private }
MenuMorph >> morphicLayerNumber [

	"helpful for insuring some morphs always appear in front of or behind others.
	smaller numbers are in front"
	^self valueOfProperty: #morphicLayerNumber  ifAbsent: [
		stayUp ifTrue:[100] ifFalse:[10]
	]
]

{ #category : #events }
MenuMorph >> mouseDown: evt [
	"Handle a mouse down event."
	"Overridden to not grab on mouse down"
	(stayUp or:[self fullContainsPoint: evt position]) 
		ifFalse:[^self deleteIfPopUp: evt]. "click outside"
	self comeToFront
]

{ #category : #'dropping/grabbing' }
MenuMorph >> mouseDownInTitle: evt [
	"Handle a mouse down event in the title bar."
	"Grab the menu and drag it to some other place"
	evt hand grabMorph: self.
]

{ #category : #events }
MenuMorph >> mouseUp: evt [
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: evt position) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		evt hand releaseMouseFocus: self.
		^self deleteIfPopUp: evt].
	stayUp ifFalse:[
		"Still in pop-up transition; keep focus"
		evt hand newMouseFocus: self].
]

{ #category : #'keyboard control' }
MenuMorph >> moveDown: evt [ 
	^self moveSelectionDown: 1 event: evt
]

{ #category : #'keyboard control' }
MenuMorph >> moveSelectionDown: direction event: evt [
	"Move the current selection up or down by one, presumably under keyboard control.
	direction = +/-1"

	| index |
	index := (submorphs indexOf: selectedItem ifAbsent: [1-direction]) + direction.
	submorphs do: "Ensure finite"
		[:unused | | m | m := submorphs atWrap: index.
		((m isMenuItemMorph) and: [m isEnabled]) ifTrue:
			[^ self selectItem: m event: evt].
		"Keep looking for an enabled item"
		index := index + direction sign].
	^ self selectItem: nil event: evt
]

{ #category : #'keyboard control' }
MenuMorph >> moveUp: evt [ 
	^self moveSelectionDown: -1 event: evt
]

{ #category : #events }
MenuMorph >> pinboxClicked [

	self stayUp: true.
	"todo improve (make the pinbox toggleable perhaps?"
]

{ #category : #events }
MenuMorph >> popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem [ 
	"Present this menu at the given point under control of the given  
	hand."
	| tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner := sourceItem.
	self fullBounds.
self updateColor.
	"ensure layout is current"
	selectedOffset := selectedItem ifNil: [
		self items 
			ifEmpty: [ 0@0 ]
			ifNotEmpty: [ :col | col first position - self position. ]
	] ifNotNil: [ selectedItem position - self position ].
					
	tryToPlace := [:where :mustFit | | delta | 
			self position: where - selectedOffset.
			delta := self fullBoundsInWorld amountToTranslateWithin: sourceItem worldBounds.
			(delta x = 0
					or: [mustFit])
				ifTrue: [delta = (0 @ 0)
						ifFalse: [self position: self position + delta].
					sourceItem world addMorphFront: self.
					^ self]].
	tryToPlace value: rightOrLeftPoint first value: false;
		 value: rightOrLeftPoint last - (self width @ 0) value: false;
		 value: rightOrLeftPoint first value: true
]

{ #category : #control }
MenuMorph >> popUpAt: aPoint forHand: hand in: aWorld [
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: self menuKeyboardControl
]

{ #category : #control }
MenuMorph >> popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean [ 
	"Present this menu at the given point under control of the given  
	hand."
	| evt |
	aWorld submorphs
		select: [ :each | (each isKindOf: MenuMorph)
			and: [each stayUp not]]
		thenCollect: [ :menu | menu delete].

	self items isEmpty
		ifTrue: [^ self].

	self layoutItems.
	"precompute width"
	self
		positionAt: aPoint
		relativeTo: (selectedItem
				ifNil: [self items first])
		inWorld: aWorld.
	aWorld addMorphFront: self.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean
		ifTrue: [hand newKeyboardFocus: self].
	evt := hand lastEvent.
	(evt isKeyboard
			or: [evt isMouse
					and: [evt anyButtonPressed not]])
		ifTrue: ["Select first item if button not down"
			self moveSelectionDown: 1 event: evt].
	self updateColor.
	self changed
]

{ #category : #control }
MenuMorph >> popUpEvent: evt in: aWorld [
	"Present this menu in response to the given event."

	| aHand aPosition |
	aHand := evt ifNotNil: [evt hand] ifNil: [ActiveHand].
	aPosition := aHand position truncated.
	^ self popUpAt: aPosition forHand: aHand in: aWorld

]

{ #category : #control }
MenuMorph >> popUpForHand: hand in: aWorld [

	| p |
	"Present this menu under control of the given hand."
	p := hand position truncated.
	^self popUpAt: p forHand: hand in: aWorld

]

{ #category : #control }
MenuMorph >> popUpInWorld [
	"Present this menu in the current World"

	^ self popUpInWorld: self currentWorld
]

{ #category : #control }
MenuMorph >> popUpInWorld: aWorld [
	"Present this menu under control of the given hand."
	^self popUpAt: aWorld primaryHand position forHand: aWorld primaryHand in: aWorld

]

{ #category : #control }
MenuMorph >> popUpNoKeyboard [
	"Present this menu in the current World, *not* allowing keyboard input into the menu"

	^ self popUpAt: ActiveHand position forHand: ActiveHand in: self currentWorld allowKeyboard: false
]

{ #category : #accessing }
MenuMorph >> popUpOwner [
	"Return the current pop-up owner that is the menu item that automatically initiated the receiver."
	^popUpOwner

]

{ #category : #accessing }
MenuMorph >> popUpOwner: aMenuItemMorph [
	"Set the current pop-up owner"
	popUpOwner := aMenuItemMorph.

]

{ #category : #private }
MenuMorph >> positionAt: aPoint relativeTo: aMenuItem inWorld: aWorld [
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |	
	self fullBounds. "force layout"
	i := 0.
	yOffset := 0.
	[(sub := self submorphs at: (i := i + 1)) == aMenuItem]
		whileFalse: [yOffset := yOffset + sub height].

	self position: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	self right > aWorld worldBounds right
		ifTrue:
			[self right: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta := self bounds amountToTranslateWithin:
		(aWorld worldBounds withHeight: ((aWorld worldBounds height - 18) max: (ActiveHand position y) + 1)).
	delta = (0 @ 0) ifFalse: [self position: self position + delta]
]

{ #category : #events }
MenuMorph >> recordFiltering: matchString [
	self setProperty: #matchString toValue: matchString
]

{ #category : #'keyboard control' }
MenuMorph >> removeMatchString [
	"Remove the matchString, if any."
	self setProperty: #matchString toValue: String new.
	self displayFiltered: nil
]

{ #category : #menu }
MenuMorph >> removeStayUpItems [

	self menuItems
		select: [ :menuItem | menuItem isStayUpItem ]
		thenDo: [ :each | each delete ].

]

{ #category : #private }
MenuMorph >> restoreFocus: originalFocusHolder in: aWorld [
	"restore only if current focus holder is nil "

	aWorld primaryHand keyboardFocus ifNotNil: [ ^ self ].
	originalFocusHolder ifNotNil: [ aWorld primaryHand newKeyboardFocus: originalFocusHolder ]
]

{ #category : #events }
MenuMorph >> rightArrowStroked: evt [
	"right arrow key"

	(selectedItem notNil and: [ selectedItem hasSubMenu ])
		ifTrue: [ evt hand newMouseFocus: selectedItem subMenu.
			selectedItem subMenu moveSelectionDown: 1 event: evt.
			evt hand newKeyboardFocus: selectedItem subMenu.
			^ true ].
		
	^ false
]

{ #category : #accessing }
MenuMorph >> rootMenu [
	popUpOwner ifNil: [^ self].
	popUpOwner owner ifNil: [^ self].
	^ popUpOwner owner rootMenu
]

{ #category : #control }
MenuMorph >> selectItem: aMenuItem event: anEvent [
	selectedItem ifNotNil:[selectedItem deselect: anEvent].
	selectedItem := aMenuItem.
	selectedItem ifNotNil:[selectedItem select: anEvent].
]

{ #category : #private }
MenuMorph >> selectedItem [
	^selectedItem
]

{ #category : #private }
MenuMorph >> setDefaultParameters [
	"change the receiver's appareance parameters"

	self
		color: self theme settings derivedMenuColor;
		borderWidth: self theme menuBorderWidth;
		borderColor: self theme menuBorderColor.
	 self theme settings flatMenu 
		ifFalse: [
			self borderStyle: BorderStyle thinGray.
			self
				hasDropShadow: true;
				shadowColor:  self theme menuShadowColor;
				shadowOffset: 1 @ 1].
	self layoutInset: 3.
	self cellInset: 0@1.

]

{ #category : #menu }
MenuMorph >> setInvokingView: invokingView [
	"Re-work every menu item of the form
		<target> perform: <selector>
	to the form
		<target> perform: <selector> orSendTo: <invokingView>.
	This supports MVC's vectoring of non-model messages to the editPane."
	self items do:
		[:item |
		item hasSubMenu 
			ifTrue: [ item subMenu setInvokingView: invokingView]
			ifFalse: [ item arguments isEmptyOrNil ifTrue:  "only the simple messages"
						[item arguments: (Array with: item selector with: invokingView).
						item selector: #perform:orSendTo:]]]
]

{ #category : #menu }
MenuMorph >> setTarget: evt [ 
	"Set the default target object to be used for add item commands, and re-target all existing items to the new target or the the invoking hand."

	| oldDefaultTarget |
	oldDefaultTarget := defaultTarget .
	oldDefaultTarget ~~ defaultTarget 
		ifTrue: [
			
			self updateItemsWithTarget: defaultTarget orWithHand: evt hand 
		].
	
]

{ #category : #accessing }
MenuMorph >> stayUp [

	^ stayUp

]

{ #category : #accessing }
MenuMorph >> stayUp: aBoolean [

	stayUp := aBoolean.
	
	titleMorph ifNotNil: [ titleMorph updatePinForm ].
]

{ #category : #events }
MenuMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #menu }
MenuMorph >> target: aMorph [
"Set defaultTarget since thats what we got.
For the sake of targetSighting which assumes #target is a word we know."

	defaultTarget := aMorph
]

{ #category : #actions }
MenuMorph >> themeChanged [
	"Update the colour if specified."

	self color: (self theme menuColorFor: nil).
	super themeChanged
]

{ #category : #actions }
MenuMorph >> toggleStayUp [

	^stayUp := stayUp not
]

{ #category : #menu }
MenuMorph >> toggleStayUp: evt [
	"Toggle my 'stayUp' flag and adjust the menu item to reflect its new state."

	self items do: [:item |
		item isStayUpItem ifTrue:
			[self stayUp: stayUp not.	
			 stayUp
				ifTrue: [item contents: 'dismiss this menu']
				ifFalse: [item contents: 'keep this menu up']]].
	evt hand releaseMouseFocus: self.
	stayUp ifFalse: [self topRendererOrSelf delete].

]

{ #category : #events }
MenuMorph >> updateColor [
	"Update the color of the menu."

	self theme preferGradientFill
		ifFalse: [ ^ self ].
	self fillStyle: (self theme menuFillStyleFor: self).	
	"update the title color"
	
	titleMorph ifNotNil: [ :tm | tm fillStyle: (self theme menuTitleFillStyleFor: tm )]

]

{ #category : #menu }
MenuMorph >> updateItemsWithTarget: aTarget orWithHand: aHand [
	"re-target all existing items"
	self items do: 
			[:item | item target ifNotNil: [
			item target isHandMorph 
				ifTrue: [item target: aHand]
				ifFalse: [item target: aTarget] ] ]
]

{ #category : #copying }
MenuMorph >> veryDeepFixupWith: deepCopier [
	"If fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals."

super veryDeepFixupWith: deepCopier.
defaultTarget := deepCopier references at: defaultTarget ifAbsent: [defaultTarget].
popUpOwner := deepCopier references at: popUpOwner ifAbsent: [popUpOwner].
activeSubMenu := deepCopier references at: activeSubMenu ifAbsent:[activeSubMenu].
]

{ #category : #copying }
MenuMorph >> veryDeepInner: deepCopier [
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

	super veryDeepInner: deepCopier.
	"defaultTarget := defaultTarget.		Weakly copied"
	selectedItem := selectedItem veryDeepCopyWith: deepCopier.
	stayUp := stayUp veryDeepCopyWith: deepCopier.
	popUpOwner := popUpOwner.		"Weakly copied"
	activeSubMenu := activeSubMenu. "Weakly copied"

]

{ #category : #control }
MenuMorph >> wantsToBeDroppedInto: aMorph [
	"Return true if it's okay to drop the receiver into aMorph.  A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object."

	^ (aMorph isWorldMorph or: [submorphs size = 1]) or: [self embeddable]
]

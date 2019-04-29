"
ComposablePresenter is an abstract class which represent a applicative presenter made to be composed with other ComposablePresenter

Most often, I am used to display other subclasses of ComposablePresenter or subclasses of AbstractWidgetPresenter.

I collaborate with 
-  ValueHolder to managed some of my variables.
- SpecFocusOrder when no focusOrder are specified.
- SpecLayout
- WindowPresenter or DialogWindowPresenter to display myself.

A new subclass of ComposablePresenter must at least define initializeWidgets and defaultSpec on the class side.

I provide the following methods
- aboutText: set the text displayed in the About.
- title: set the title of the window.
- extent: set the initial size of the window, alternative possibility is define again initialExtent, it must be called before have displayed the widget. 
- focusOrder: set an instance what manage the order of focus of the keyboard navigation.
- bindKeyCombination:toAction: create a shortcut on keyboard what perform the block of the  action.
- owner getter of my parent.
- window getter of the window displaying me.

On my class side, I provide methods returning standard height and width for some elements.

todo
- announce:
- applyMenuModel: and neglectMenuModel:
- keyStrokeForNextFocus: , giveFocusToNextFrom: and takeKeyboardFocus
- ensureExtentFor:
- hide
- needRebuild
- on:do:
- update:
- widget

Window
=======

- openWithSpec instantiate a WindowPresenter using the lookup to found the layout to use.
- openDialogWithSpec instanciate a DialogWindowPresenter using the lookup to found the layout to use.
Their variants openWithSpec: and openDialogWithSpec:  use the layout parameter.

These methods can be useful to manage the window
- isDisplayed return true if the window of the widget is displayed
- hasWindow return true if the widget have a window
- centered to center the window in the world.
- delete to delete the window.

todo
- cancelled
- setModal:
- windowIcon:


Instantiation
===========

* initializeWidgets is called by the initialize method, it should contain initialization of subwidgets and of the focusorder.

The instantiation of a subwidget should use one of that way
- instantiate: take a class in parameter and return the created instance.
- methods named 'new' followed by a widget name are shortcut working with instatiate:

Usually, the subwidgets must be added in the focusOrder using something like 'self focusOrder add: accessor of  the  subwidget'

Note that instantiateModels: is legacy code in ComposablePresenter and must not be used. It will be deprecated and removed.

* initializePresenter is called by the initialize method after initializeWidgets, it should manage interaction of subwidgets.

Methods named 'when' followed by an event provide hook to perform the action in parameter.

Note
-------
Be careful about code order if you are overriding initialize method.
Normally in Spec initializing instance variables should be done BEFORE calling super initialize (so the opposite of the normal approach), because super initialize calls initalizeWidgets and initializePresenter that normally would make use of those variables.

Layout
======

See SpecLayout

defaultSpec or a method containing the pragma <spec: #default> must be defined in the class side of my subclasses.
It contains informations about how place its elements.
It possible to define more than one method to give the possibility to use another layout, by default the one containing the pragma will be used if it exists, if not defaultSpec will be used.
"
Class {
	#name : #ComposablePresenter,
	#superclass : #Presenter,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'#layout',
		'#application',
		'#focusOrder',
		'#extentHolder',
		'#additionalKeyBindings',
		'#keyStrokesForNextFocusHolder',
		'#keyStrokesForPreviousFocusHolder',
		'#windowIcon',
		'#aboutText',
		'#askOkToClose',
		'#titleHolder',
		'#inputPorts',
		'#outputPorts',
		'#transmissions',
		'#additionalSubpresentersMap',
		'#visible => SpecObservableSlot'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #specs }
ComposablePresenter class >> defaultSpec [

	^ self subclassResponsibility
]

{ #category : #accessing }
ComposablePresenter class >> iconNamed: aSymbol [
	^ Smalltalk ui icons iconNamed: aSymbol
]

{ #category : #'instance creation' }
ComposablePresenter class >> newApplication: anApplication [

	^ self basicNew
		application: anApplication;
		initialize;
		yourself
]

{ #category : #'instance creation' }
ComposablePresenter class >> newApplication: anApplication model: aModel [

	^ self basicNew
		application: anApplication;
		setModelBeforeInitialization: aModel;
		initialize;
		yourself
]

{ #category : #'instance creation' }
ComposablePresenter class >> newApplication: anApplication owner: anOwningPresenter [

	^ self basicNew
		application: anApplication;
		owner: anOwningPresenter;
		initialize;
		yourself
]

{ #category : #'instance creation' }
ComposablePresenter class >> newApplication: anApplication owner: anOwningPresenter model: aDomainObject [

	^ self basicNew
		application: anApplication;
		owner: anOwningPresenter;
		setModelBeforeInitialization: aDomainObject;
		initialize;
		yourself
]

{ #category : #'instance creation' }
ComposablePresenter class >> on: aDomainObject [

	"self 
		deprecated: 'ComposablePresenters are instantiated in the context of an application!' 
		on: '2019-02-26' 
		in: #Pharo8."
		
	^ self 
		newApplication: SpecNullApplication new 
		model: aDomainObject 
]

{ #category : #'instance creation' }
ComposablePresenter class >> owner: anOwningPresenter on: aDomainObject [

	^ self basicNew
		owner: anOwningPresenter;
		setModelBeforeInitialization: aDomainObject;
		initialize;
		yourself
]

{ #category : #'as yet unclassified' }
ComposablePresenter class >> systemIconName [

	^ #smallWindow
]

{ #category : #specs }
ComposablePresenter class >> title [
	
	^ 'Untitled window'
]

{ #category : #'api-window' }
ComposablePresenter >> aboutText [

	^ aboutText value ifNil: [ aboutText value: 'The about text for this window has not been set.'; value]
]

{ #category : #'api-window' }
ComposablePresenter >> aboutText: aString [

	aboutText value: aString
]

{ #category : #private }
ComposablePresenter >> addAll: aWindow withSpec: aSpec [

	aWindow
		addMorph: (self buildWithSpec: aSpec)
		frame: (0@0 corner: 1@1).
]

{ #category : #private }
ComposablePresenter >> addAll: aWindow withSpecLayout: aSpec [

	aWindow
		addMorph: (self buildWithSpecLayout: aSpec)
		frame: (0@0 corner: 1@1).
]

{ #category : #'private-ports' }
ComposablePresenter >> addInputPort: aPort [

	aPort presenter: self.
	inputPorts ifNil: [ inputPorts := #() ].
	inputPorts := inputPorts copyWith: aPort
]

{ #category : #'window menu' }
ComposablePresenter >> addMenuItemsToWindowMenu: aMenu [
	"Do nothing"
]

{ #category : #'private-ports' }
ComposablePresenter >> addOutputPort: aPort [

	aPort presenter: self.
	outputPorts ifNil: [ outputPorts := #() ].
	outputPorts := outputPorts copyWith: aPort
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> additionalKeyBindings [

	^ additionalKeyBindings
]

{ #category : #TOREMOVE }
ComposablePresenter >> additionalSubpresentersMap [

	"additionalSubpresentersMap serves for storing of sub-presenters that are not
	stored in instance variables"
	
	self flag: #TOREMOVE. "This is now provided by TDynamicPresenter"
	^ additionalSubpresentersMap ifNil: [ additionalSubpresentersMap := Dictionary new ]
]

{ #category : #'private-traversing' }
ComposablePresenter >> allPresenters [
	| allPresenters |

	allPresenters := OrderedCollection new.
	self traversePresentersDo: [ :each | 
		allPresenters add: each ].
	^ allPresenters
]

{ #category : #api }
ComposablePresenter >> announce: anAnnouncement [

	self announcer announce: anAnnouncement
]

{ #category : #accessing }
ComposablePresenter >> application [

	^ self hasOwner 
		ifTrue: [ self owner application ]
		ifFalse: [
			application ifNil: [ 
				self flag: #TODO. "This is incorrect, no component who is also a root
				(it does not has owner) should not have an application. But in older times
				it was not like that and to support such components we assign them to a 
				default application"
				application := SpecApplication defaultApplication ] ]
]

{ #category : #accessing }
ComposablePresenter >> application: anApplication [

	application := anApplication
]

{ #category : #'window menu' }
ComposablePresenter >> applyMenuModel: aMenuModel [

	aMenuModel menuGroups do: [ :group |
		group menuItems do: [ :item |
			item shortcut ifNotNil: [ :shortcut | self bindMenuKeyCombination: shortcut toAction: [ item performMenuActionWith: {} ] ].
			item subMenu ifNotNil: [ :subMenu | subMenu applyTo: self	] ] ]
]

{ #category : #converting }
ComposablePresenter >> asPresenter [
	"This allows to use presenter instances inside layouts directly"
	^ self
]

{ #category : #'api-window' }
ComposablePresenter >> askOkToClose [

	^ askOkToClose value
]

{ #category : #'api-window' }
ComposablePresenter >> askOkToClose: aBoolean [
	self flag: #TODO. "Move this to WindowPresenter (it should be added in 
	#initializeWindow:"
	askOkToClose value: aBoolean
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> bindKeyCombination: aShortcut toAction: aBlock [

	self additionalKeyBindings at: aShortcut put: aBlock.
	
	self changed: #bindKeyCombination:toAction: with: {aShortcut . aBlock}
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> bindMenuKeyCombination: aShortcut toAction: aBlock [

	self additionalKeyBindings at: aShortcut put: aBlock.
	
	self changed: #bindMenuKeyCombination:toAction: with: {aShortcut . aBlock}
]

{ #category : #'api-window' }
ComposablePresenter >> cancelled [
	self flag: #TODO. "just for dialogs... I wonder if we need it here?"

	self withWindowDo: #cancelled
]

{ #category : #TOMOVE }
ComposablePresenter >> centerWidget: aWindow [

	self changed: #centerWidget: with: { aWindow }
]

{ #category : #'api-window' }
ComposablePresenter >> centered [

	self withWindowDo: #centered
]

{ #category : #TOMOVE }
ComposablePresenter >> centeredRelativeTo: aModel [

	self withWindowDo: [ :w | 
		w centeredRelativeTo: aModel ]
]

{ #category : #private }
ComposablePresenter >> createInstanceFor: aClassSymbol [
	"Retrieve the class corresponding to aClassSymbol using the bindings, then create a new instance of theis class"
	| class |
	
	class := self resolveSymbol: aClassSymbol.
	^ self instantiate: class.
]

{ #category : #specs }
ComposablePresenter >> defaultSpec [

	^ self class perform: self defaultSpecSelector
]

{ #category : #'api-showing' }
ComposablePresenter >> defaultWindowPresenterClass [

	^ WindowPresenter
]

{ #category : #private }
ComposablePresenter >> delete [

	self withWindowDo: #delete
]

{ #category : #accessing }
ComposablePresenter >> enabled: aBoolean [

	self presentersDo: [ :each | 
		each enabled: aBoolean ]
]

{ #category : #TOMOVE }
ComposablePresenter >> ensureExtentFor: widget [

	self extent
		ifNil: [ self initialExtent
			ifNotNil: [ :ex | 
				(widget respondsTo: #extent:)
					ifTrue: [ widget extent: ex ] ] ]
		ifNotNil: [ :ex | 
			(widget respondsTo: #extent:)
				ifTrue: [ widget extent: ex ] ].
	
]

{ #category : #'private-focus' }
ComposablePresenter >> ensureKeyBindingsFor: aWidget [

	self registerKeyStrokesForNextFor: aWidget.
	self registerKeyStrokesForPreviousFor: aWidget.
	
	self additionalKeyBindings associationsDo: [:association | aWidget bindKeyCombination: association key toAction: association value ]
]

{ #category : #'api-focus' }
ComposablePresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"

	^ keyStrokesForNextFocusHolder value
]

{ #category : #'api-focus' }
ComposablePresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"

	^ keyStrokesForPreviousFocusHolder value
]

{ #category : #TOMOVE }
ComposablePresenter >> extent [

	^  extentHolder value
]

{ #category : #TOMOVE }
ComposablePresenter >> extent: aPoint [

	^  extentHolder value: aPoint
]

{ #category : #accessing }
ComposablePresenter >> focusOrder [

	^ focusOrder ifNil: [ focusOrder := OrderedCollection new ].
]

{ #category : #accessing }
ComposablePresenter >> focusOrder: anObject [
	^ focusOrder := anObject
]

{ #category : #'private-focus' }
ComposablePresenter >> giveFocusToNextFrom: aModel [
	| focus |
	
	focus := self focusOrder.
	^ true
]

{ #category : #'private-focus' }
ComposablePresenter >> giveFocusToPreviousFrom: aModel [ 
	| focus |
	
	focus := self focusOrder.

	(focus includes: aModel)
		ifTrue: [
			| index previous |
			index := (focus indexOf: aModel) - 1.
			(index > 0)
				ifTrue: [
					previous := focus at: index.
					previous takeLastKeyboardFocus.
					^ true ]].

	^ owner 
		ifNil: [ " I loop "
			| next |
			next := focus at: focus size ifAbsent: [ ^ false ].
			next takeLastKeyboardFocus.
			true]
		ifNotNil: [ 
			owner giveFocusToPreviousFrom: self.
			true ]
]

{ #category : #'private-focus' }
ComposablePresenter >> handlesKeyboard: evt [

	^ true
]

{ #category : #api }
ComposablePresenter >> hide [

	visible := false
]

{ #category : #accessing }
ComposablePresenter >> iconNamed: aSymbol [

	^ self class iconNamed: aSymbol
]

{ #category : #'api-window' }
ComposablePresenter >> initialExtent [

	^ nil
]

{ #category : #initialization }
ComposablePresenter >> initialize [
	self class initializeSlots: self.
	super initialize.

	additionalSubpresentersMap := nil.
	visible := true.

	self initializePorts.
	self initializePrivateAnnouncer.
	self initializePrivateValueHolders.
	self initializePrivateHooks.
	self initializePrivateAnnouncements.
]

{ #category : #initialization }
ComposablePresenter >> initializePorts [
	
	"By default, all presenters can receive a change model transmission"
	self addInputPort: ModelPresenterPort new
]

{ #category : #initialization }
ComposablePresenter >> initializePresenter [
	"Used to specify the subwidgets, and/or to bind them together"

	"By default, do not do anything"

	extentHolder whenChangedDo: [ :ex | 
		self withAdapterDo: [ :widget | 
			(widget respondsTo: #extent:)
				ifTrue: [ widget extent: ex ] ] ]
]

{ #category : #'initialization - private' }
ComposablePresenter >> initializePrivateAnnouncements [

	keyStrokesForNextFocusHolder whenChangedDo: [ self registerKeyStrokesForNextFor: self widget ].
	keyStrokesForPreviousFocusHolder whenChangedDo: [ self registerKeyStrokesForPreviousFor: self widget ].
	titleHolder whenChangedDo: [ self updateTitle ]
]

{ #category : #'initialization - private' }
ComposablePresenter >> initializePrivateAnnouncer [

  announcer := Announcer new.

]

{ #category : #'initialization - private' }
ComposablePresenter >> initializePrivateHooks [

	self initializeWidgets.
	self initializePresenter.
	self updatePresenter.

]

{ #category : #'initialization - private' }
ComposablePresenter >> initializePrivateValueHolders [

	extentHolder := nil asValueHolder.
	keyStrokesForNextFocusHolder := { KMNoShortcut new } asValueHolder.
	keyStrokesForPreviousFocusHolder := { KMNoShortcut new } asValueHolder.
	additionalKeyBindings := Dictionary new.
	aboutText := nil asValueHolder.
	windowIcon := nil asValueHolder.
	askOkToClose := false asValueHolder.
	titleHolder := self class title asValueHolder.
	

]

{ #category : #initialization }
ComposablePresenter >> initializeWidgets [
	
	self subclassResponsibility
]

{ #category : #initialization }
ComposablePresenter >> initializeWindow: aWindowPresenter [
	"override this to set window values before opening. 
	 You may want to add a menu, a toolbar or a statusbar"
	
	aWindowPresenter 
		title: self title;
		initialExtent: self initialExtent;
		windowIcon: self windowIcon;
		askOkToClose: self askOkToClose;
		aboutText: self aboutText
]

{ #category : #'private-ports' }
ComposablePresenter >> inputPortDefault [

	"first input port will be default. 
	 this may be bad design and probably needs to be revisited later"
	^ self inputPorts first
]

{ #category : #'private-ports' }
ComposablePresenter >> inputPortNamed: aSymbol [

	^ self inputPorts detect: [ :each | each name = aSymbol ]
]

{ #category : #'private-ports' }
ComposablePresenter >> inputPorts [

	^ inputPorts
]

{ #category : #'instance creation' }
ComposablePresenter >> instantiate: aComposablePresenterClass [
	"Instantiate a ComposablePresenter subclass and set its instance owner"
	^ aComposablePresenterClass owner: self
]

{ #category : #'instance creation' }
ComposablePresenter >> instantiate: aComposablePresenterClass on: aModel [
	"Instantiate a ComposablePresenter subclass and set its instance owner and model"
	^ aComposablePresenterClass owner: self on: aModel
]

{ #category : #'instance creation' }
ComposablePresenter >> instantiatePresenters: aCollectionOfPairs [
	"instantiatePresenters: is legacy code in ComposablePresenter and must not be used. It will be deprecated and removed."

	"Used to instantiate multiple sub widget at once. Take aCollectionOfPairs where each odd element is an inst var name and each even element is a class name, create an instance from the class name and store it into the inst var"

	(aCollectionOfPairs anySatisfy: [ :e | e isKindOf: Association ])
		ifTrue: [ aCollectionOfPairs
				do: [ :a | 
					| k v |
					k := a key.
					v := a value.
					self instVarNamed: k asString put: (self createInstanceFor: v) ] ]
		ifFalse: [ aCollectionOfPairs
				pairsDo: [ :k :v | self instVarNamed: k asString put: (self createInstanceFor: v) ] ]
]

{ #category : #api }
ComposablePresenter >> isDisplayed [
	"Return true if the widget is currently displayed on screen"
	
	self flag: #TODO. "To move to WindowPresenter exclusive?"
	^ self hasWindow 
		ifTrue: [ self root isDisplayed ]
		ifFalse: [ false ]
]

{ #category : #testing }
ComposablePresenter >> isVisible [
	
	^ visible value
]

{ #category : #'api-focus' }
ComposablePresenter >> keyStrokeForNextFocus: aKMShortcut [

	keyStrokesForNextFocusHolder value: { aKMShortcut }
]

{ #category : #'api-focus' }
ComposablePresenter >> keyStrokeForPreviousFocus: aKMShortcut [

	keyStrokesForPreviousFocusHolder value: { aKMShortcut }
]

{ #category : #'api-focus' }
ComposablePresenter >> keyStrokesForNextFocus: aCollection [

	keyStrokesForNextFocusHolder value: aCollection
]

{ #category : #'api-focus' }
ComposablePresenter >> keyStrokesForPreviousFocus: aCollection [

	keyStrokesForPreviousFocusHolder value: aCollection 
]

{ #category : #accessing }
ComposablePresenter >> layout [
	^ layout
]

{ #category : #'window menu' }
ComposablePresenter >> neglectMenuModel: aMenuModel [

	aMenuModel menuGroups do: [ :group |
		group menuItems do: [ :item |
			item shortcut ifNotNil: [ :shortcut | self removeMenuKeyCombination: shortcut ].
			item subMenu ifNotNil: [ :subMenu | subMenu neglect: self	] ] ]
]

{ #category : #widgets }
ComposablePresenter >> newActionBar [

	^ self instantiate: ActionBarPresenter
]

{ #category : #widgets }
ComposablePresenter >> newButton [
	^ self instantiate: ButtonPresenter
]

{ #category : #widgets }
ComposablePresenter >> newButtonBar [

	^ self instantiate: ButtonBarPresenter
]

{ #category : #widgets }
ComposablePresenter >> newCheckBox [
	^ self instantiate: CheckBoxPresenter
]

{ #category : #widgets }
ComposablePresenter >> newCode [
	^ self instantiate: CodePresenter
]

{ #category : #widgets }
ComposablePresenter >> newDiff [
	^ self instantiate: DiffPresenter
]

{ #category : #widgets }
ComposablePresenter >> newDropList [
	^ self instantiate: DropListPresenter
]

{ #category : #widgets }
ComposablePresenter >> newDynamicPresentersListIn: accessorSymbol usingBuilder: aDynamicPresentersListBuilder [
	|mutatorSymbol newDynamicPresenter |
	mutatorSymbol := (accessorSymbol , ':') asSymbol.
	(self perform: accessorSymbol)
		ifNotNil: [ :dynamicPresenter | dynamicPresenter owner: nil ].
	newDynamicPresenter := aDynamicPresentersListBuilder build.
	
	self perform: mutatorSymbol with: newDynamicPresenter.
	
	newDynamicPresenter owner: self.
	
	self needRebuild: false.
	self buildWithSpec
]

{ #category : #widgets }
ComposablePresenter >> newImage [
	^ self instantiate: ImagePresenter
]

{ #category : #widgets }
ComposablePresenter >> newLabel [
	^ self instantiate: LabelPresenter
]

{ #category : #widgets }
ComposablePresenter >> newLink [
	^ self instantiate: LinkPresenter
]

{ #category : #widgets }
ComposablePresenter >> newList [
	^ self instantiate: ListPresenter
]

{ #category : #widgets }
ComposablePresenter >> newMenu [
	^ self instantiate: MenuPresenter
]

{ #category : #widgets }
ComposablePresenter >> newMenuBar [
	^ self instantiate: MenuBarPresenter
]

{ #category : #widgets }
ComposablePresenter >> newNotebook [

	^ self instantiate: NotebookPresenter
]

{ #category : #widgets }
ComposablePresenter >> newNotebookPage [

	self flag: #TODO. "NotebookPage needs to be a Presenter?"
	^ NotebookPage new
]

{ #category : #widgets }
ComposablePresenter >> newNullPresenter [
	^ self instantiate: NullPresenter
]

{ #category : #widgets }
ComposablePresenter >> newNumberInput [
	^ self instantiate: NumberInputFieldPresenter
]

{ #category : #widgets }
ComposablePresenter >> newProgressBar [
	^ self instantiate: ProgressBarPresenter
]

{ #category : #widgets }
ComposablePresenter >> newRadioButton [
	^ self instantiate: RadioButtonPresenter
]

{ #category : #widgets }
ComposablePresenter >> newSlider [
	^ self instantiate: SliderPresenter
]

{ #category : #widgets }
ComposablePresenter >> newTable [

	^ self instantiate: TablePresenter
]

{ #category : #widgets }
ComposablePresenter >> newText [
	^ self instantiate: TextPresenter
]

{ #category : #widgets }
ComposablePresenter >> newTextInput [
	^ self instantiate: TextInputFieldPresenter
]

{ #category : #widgets }
ComposablePresenter >> newToolbarPresenter [

	^ self instantiate: ToolBarPresenter
]

{ #category : #widgets }
ComposablePresenter >> newTreeTable [

	^ self instantiate: TreeTablePresenter
]

{ #category : #private }
ComposablePresenter >> okToChange [

	^ self hasWindow
		ifTrue: [ self window okToChange ]
		ifFalse: [ true ] 
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> on: aShortcut do: aBlock [

	self bindKeyCombination: aShortcut toAction: aBlock
]

{ #category : #'api-announcements' }
ComposablePresenter >> on: anAnnouncement send: aSelector to: aTarget [

	self announcer
		when: anAnnouncement 
		send: aSelector 
		to: aTarget
]

{ #category : #'private-ports' }
ComposablePresenter >> outputPortDefault [

	"first output port will be default. 
	 this may be bad design and probably needs to be revisited later"
	^ self outputPorts first
]

{ #category : #'private-ports' }
ComposablePresenter >> outputPortNamed: aSymbol [

	^ self outputPorts detect: [ :each | each name = aSymbol ]
]

{ #category : #'private-ports' }
ComposablePresenter >> outputPorts [

	^ outputPorts
]

{ #category : #accessing }
ComposablePresenter >> presenterAt: aName [
	"Retrieves a subpresenter of this composed presenter."

	^ self readSlotNamed: aName
]

{ #category : #accessing }
ComposablePresenter >> presenterAt: aName ifAbsent: aBlock [
	"Retrieves a subpresenter of this composed presenter."

	^ [ self readSlotNamed: aName ]
		on: SlotNotFound
		do: aBlock
]

{ #category : #'private-traversing' }
ComposablePresenter >> presenters [
	| allPresenters |

	allPresenters := OrderedCollection new.
	self presentersDo: [ :each | 
		allPresenters add: each ].
	^ allPresenters
]

{ #category : #'private-traversing' }
ComposablePresenter >> presentersDo: aBlock [

	self class allSlots 
		reject: [ :each | each name = #owner ] 
		thenDo: [ :each | | var |
			var := self readSlot: each.
			(var isKindOf: Presenter) ifTrue: [ 
				aBlock value: var ] ]
]

{ #category : #'private-focus' }
ComposablePresenter >> registerKeyStrokesForNextFor: aWidget [

	aWidget ifNil: [ ^ self ].
	
	self eventKeyStrokesForNextFocus do: [:each |
		aWidget bindKeyCombination: each toAction: [ self giveFocusToNextFrom: self ] ]
]

{ #category : #'private-focus' }
ComposablePresenter >> registerKeyStrokesForPreviousFor: aWidget [

	aWidget ifNil: [ ^ self ].
	
	self eventKeyStrokesForPreviousFocus do: [:each |
		aWidget bindKeyCombination: each toAction: [ self giveFocusToPreviousFrom: self ] ]
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> removeKeyCombination: aShortcut [

	self additionalKeyBindings removeKey: aShortcut ifAbsent: [ ^ self ].
	
	self changed: #removeKeyCombination: with: {aShortcut}
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> removeMenuKeyCombination: aShortcut [

	self additionalKeyBindings removeKey: aShortcut ifAbsent: [ ^ self ].
	
	self changed: #removeMenuKeyCombination: with: {aShortcut}
]

{ #category : #'api-window' }
ComposablePresenter >> requestWindowClose [

	"returns <true> if the user is allowed to close the window. Useful if you want to ask user if he wants to save the changed content etc."
	
	^ true
]

{ #category : #private }
ComposablePresenter >> resolveSymbol: aSymbol [

	^ Smalltalk at: aSymbol
]

{ #category : #api }
ComposablePresenter >> retrieveSpec: aSelector [

	self layout ifNotNil: [ ^ self layout ].
	^ super retrieveSpec: aSelector
]

{ #category : #TOMOVE }
ComposablePresenter >> setExtentAndBindingTo: widget [

	self flag: #TODO. "This is bad. Extent needs to be 'ensured' at adapter level. 
	Same for keybindings"
	self ensureExtentFor: widget.
	self ensureKeyBindingsFor: widget
]

{ #category : #'api-showing' }
ComposablePresenter >> setModal: aWindow [

	self 
		deprecated: 'Do not use this directly. Use #openModalWithSpec (and family) instead.' 
		on: '2019-02-26' 
		in: #Pharo8.
	
	self changed: #setModal: with: { aWindow }
]

{ #category : #'accessing model' }
ComposablePresenter >> setModel: aDomainObject [


]

{ #category : #'accessing model' }
ComposablePresenter >> setModelBeforeInitialization: aDomainObject [


]

{ #category : #api }
ComposablePresenter >> show [

	visible := true
]

{ #category : #TOREMOVE }
ComposablePresenter >> specSpacer [

	^ SpacerPresenter new
]

{ #category : #'private-focus' }
ComposablePresenter >> takeKeyboardFocus [

	self focusOrder ifNotEmpty:[:focus | ^ focus first takeKeyboardFocus ].
	^ self changed: #takeKeyboardFocus with: #()
]

{ #category : #'private-focus' }
ComposablePresenter >> takeLastKeyboardFocus [

	self focusOrder
		ifEmpty: [ self takeKeyboardFocus ]
		ifNotEmpty: [:focus | focus last takeKeyboardFocus ].
]

{ #category : #'api-window' }
ComposablePresenter >> title [
	"Return the window's title"

	^ titleHolder value
]

{ #category : #'api-window' }
ComposablePresenter >> title: aString [

	titleHolder value: aString
]

{ #category : #TOREMOVE }
ComposablePresenter >> toolName [

	^ self class toolName
]

{ #category : #'private-ports' }
ComposablePresenter >> transmit [
	| newTransmition |

	transmissions ifNil: [ transmissions := Set new ].
	newTransmition := PresenterTransmission on: self.
	transmissions add: newTransmition.
	^ newTransmition
]

{ #category : #'private-traversing' }
ComposablePresenter >> traversePresentersDo: aBlock [

	self 
		traversePresentersDo: aBlock 
		excluding: Set new
]

{ #category : #'private-traversing' }
ComposablePresenter >> traversePresentersDo: aBlock excluding: excludes [

	(excludes includes: self) ifTrue: [ ^ self ]. 
	excludes add: self.
	aBlock value: self.

	self class allSlots do: [ :each | | var |
		var := self readSlot: each.
		(var isKindOf: Presenter) ifTrue: [ 
			var traversePresentersDo: aBlock excluding: excludes ] ]
]

{ #category : #initialization }
ComposablePresenter >> updatePresenter [
]

{ #category : #api }
ComposablePresenter >> updateTitle [

	self flag: #TODO. "To move to WindowPresenter"
	"Update the window title"
	self withWindowDo: #updateTitle
]

{ #category : #api }
ComposablePresenter >> visibleIf: aValuable [
	visible := aValuable
]

{ #category : #'api-events' }
ComposablePresenter >> whenShortcutsChanged: aBlock [
	"Set a block to value when the shortcuts block has changed"
	
	self additionalKeyBindings whenChangedDo: aBlock
]

{ #category : #'api-events' }
ComposablePresenter >> whenVisibleChangedDo: aBlock [
	
	self property: #visible whenChangedDo: aBlock
]

{ #category : #'api-window' }
ComposablePresenter >> whenWindowChanged: aBlock [

	self flag: #TODO. "This is here for backwards compatibility (you can have same 
	functionality in #initializeWindow:"
	self announcer 
		when: WindowBuilt 
		do: [ :ann | aBlock cull: ann model ]
]

{ #category : #accessing }
ComposablePresenter >> widget [
	"I return the adapter responsible of building my widget"
	
	self deprecated: 'Should use #adapter instead'
		transformWith: '`@receiver widget' -> '`@receiver adapter'.
	
	^ self spec
]

{ #category : #api }
ComposablePresenter >> windowIcon [

	^ windowIcon value
]

{ #category : #api }
ComposablePresenter >> windowIcon: aForm [

	windowIcon value: aForm
]

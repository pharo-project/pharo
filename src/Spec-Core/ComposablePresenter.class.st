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
	#superclass : #Model,
	#instVars : [
		'focusOrder',
		'owner',
		'window',
		'spec',
		'extentHolder',
		'needRebuild',
		'additionalKeyBindings',
		'keyStrokesForNextFocusHolder',
		'keyStrokesForPreviousFocusHolder',
		'windowIcon',
		'aboutText',
		'askOkToClose',
		'titleHolder'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #defaults }
ComposablePresenter class >> buttonHeight [

	^ self defaultFont height + 12
]

{ #category : #defaults }
ComposablePresenter class >> buttonWidth [

	^ (self defaultFont widthOfString: 'eilwp') + 44
]

{ #category : #queries }
ComposablePresenter class >> defaultFont [ 

	^ self platform defaultFont
]

{ #category : #specs }
ComposablePresenter class >> defaultSpec [

	^ self subclassResponsibility
]

{ #category : #defaults }
ComposablePresenter class >> iconNamed: aSymbol [
	^ Smalltalk ui icons iconNamed: aSymbol
]

{ #category : #defaults }
ComposablePresenter class >> inputTextHeight [

	^ self defaultFont height + 12
]

{ #category : #defaults }
ComposablePresenter class >> labelHeight [

	^ self defaultFont height
]

{ #category : #'instance creation' }
ComposablePresenter class >> on: aDomainObject [

	^ self basicNew
		setModelBeforeInitialization: aDomainObject;
		initialize;
		yourself
]

{ #category : #'instance creation' }
ComposablePresenter class >> owner: anOwningPresenter [

	^ self basicNew
		owner: anOwningPresenter;
		initialize;
		yourself
]

{ #category : #'instance creation' }
ComposablePresenter class >> owner: anOwningPresenter on: aDomainObject [

	^ self basicNew
		owner: anOwningPresenter;
		setModelBeforeInitialization: aDomainObject;
		initialize;
		yourself
]

{ #category : #platform }
ComposablePresenter class >> platform [

	^ SpecInterpreter bindings
]

{ #category : #protocol }
ComposablePresenter class >> specSelectors [
	^ self class withAllSuperclasses
		flatCollect: [ :class | 
			(((SpecPragmaCollector behavior: class)
				filter: [ :pragma | pragma selector = #spec ])
				reset;
				collected) collect: [ :pragmas | pragmas method selector ] ]
]

{ #category : #specs }
ComposablePresenter class >> title [
	
	^ 'Untitled window'
]

{ #category : #defaults }
ComposablePresenter class >> toolbarHeight [

	^ self defaultFont height + 12
]

{ #category : #api }
ComposablePresenter >> aboutText [

	^ aboutText value ifNil: [ aboutText value: 'The about text for this window has not been set.'; value]
]

{ #category : #api }
ComposablePresenter >> aboutText: aString [

	aboutText value: aString
]

{ #category : #api }
ComposablePresenter >> adapterFrom: aSpecLayout model: aModel [
	^ SpecInterpreter interpretASpec: aSpecLayout model: aModel
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

{ #category : #'window menu' }
ComposablePresenter >> addMenuItemsToWindowMenu: aMenu [
	"Do nothing"
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> additionalKeyBindings [

	^ additionalKeyBindings value
]

{ #category : #api }
ComposablePresenter >> announce: anAnnouncement [

	self announcer announce: anAnnouncement
]

{ #category : #api }
ComposablePresenter >> apiSelectors [

	^ self class apiSelectors
]

{ #category : #api }
ComposablePresenter >> applyMenuModel: aMenuModel [

	aMenuModel menuGroups do: [ :group |
		group menuItems do: [ :item |
			item shortcut ifNotNil: [ :shortcut | self bindMenuKeyCombination: shortcut toAction: [ item performMenuActionWith: {} ] ].
			item subMenu ifNotNil: [ :subMenu | subMenu applyTo: self	] ] ]
]

{ #category : #api }
ComposablePresenter >> askOkToClose [

	^ askOkToClose value
]

{ #category : #api }
ComposablePresenter >> askOkToClose: aBoolean [

	askOkToClose value: aBoolean
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> bindKeyCombination: aShortcut toAction: aBlock [

	additionalKeyBindings at: aShortcut put: aBlock.
	
	self changed: #bindKeyCombination:toAction: with: {aShortcut . aBlock}
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> bindMenuKeyCombination: aShortcut toAction: aBlock [

	additionalKeyBindings at: aShortcut put: aBlock.
	
	self changed: #bindMenuKeyCombination:toAction: with: {aShortcut . aBlock}
]

{ #category : #api }
ComposablePresenter >> buildWithSpec [
	"Build the widget using the default spec"
	
	^ self buildWithSpec: self defaultSpecSelector
]

{ #category : #api }
ComposablePresenter >> buildWithSpec: aSpec [
	"Build the widget using the spec name provided as argument"
	
	^ self buildWithSpecLayout: (self retrieveSpec: aSpec)
]

{ #category : #api }
ComposablePresenter >> buildWithSpecLayout: aSpecLayout [
	"Build the widget using the spec name provided as argument"
	| widget adapter|
	adapter := self adapterFrom: aSpecLayout model: self.
	widget := adapter asWidget.
	self setExtentAndBindingTo: widget.
	^ widget
]

{ #category : #api }
ComposablePresenter >> cancelled [

	^ self window
		ifNil: [ false ]
		ifNotNil: [ :w | w cancelled ]
]

{ #category : #api }
ComposablePresenter >> centerWidget: aWindow [

	self changed: #centerWidget: with: { aWindow }
]

{ #category : #api }
ComposablePresenter >> centered [

	window value ifNotNil: [ :w | w centered ]
]

{ #category : #api }
ComposablePresenter >> centeredRelativeTo: aModel [

	window value ifNotNil: [ :w | w centeredRelativeTo: aModel ]
]

{ #category : #'instance creation' }
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

{ #category : #private }
ComposablePresenter >> defaultSpecSelector [
	self class class
		withAllSuperclassesDo: [ :class | 
			(((SpecPragmaCollector behavior: class)
				filter:
					[ :pragma | pragma selector = 'spec:' and: [ pragma arguments includes: #default ] ])
				reset;
				collected)
				ifNotEmpty: [ :pragmas | ^ pragmas first method selector ] ].
	self specSelectors
		ifNotEmpty: [ :col | 
			col size = 1
				ifTrue: [ ^ col first ] ].

	"should use pragmas"
	^ #defaultSpec
]

{ #category : #accessing }
ComposablePresenter >> defaultWindowModelClass [ 
	^ WindowPresenter
]

{ #category : #private }
ComposablePresenter >> delete [

	window value
		ifNil: [ self changed: #delete with: #() ]
		ifNotNil: [ :w | w delete ]
]

{ #category : #api }
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
	
	additionalKeyBindings associationsDo: [:association | aWidget bindKeyCombination: association key toAction: association value ]
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

{ #category : #api }
ComposablePresenter >> extent [

	^  extentHolder value
]

{ #category : #api }
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

	(focus includes: aModel)
		ifTrue: [
			| index next |
			index := (focus indexOf: aModel) + 1.
			(index <= focus size)
				ifTrue: [
					next := focus at: index.
					next takeKeyboardFocus.
					^ true ]].

	^ owner 
		ifNil: [ " I loop "
			| next |
			next := focus at: 1 ifAbsent: [ ^ false ].
			next takeKeyboardFocus.
			true ]
		ifNotNil: [ 
			owner giveFocusToNextFrom: self.
			true ]
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

{ #category : #testing }
ComposablePresenter >> hasWindow [
	"Answers true if there is an open window using this model."

	self owner ifNil: [ ^ self window ifNotNil: [ :w | w isClosed not ] ifNil: [ false ] ].
	 ^ self owner hasWindow
]

{ #category : #api }
ComposablePresenter >> hide [

	self widget ifNotNil: [:widget |
		(widget respondsTo: #hide) ifTrue: [ widget hide ]].
]

{ #category : #accessing }
ComposablePresenter >> iconNamed: aSymbol [
	^ self class iconNamed: aSymbol
]

{ #category : #api }
ComposablePresenter >> initialExtent [

	^ nil
]

{ #category : #initialization }
ComposablePresenter >> initialize [

	super initialize.

	self initializePrivateAnnouncer.
	self initializePrivateValueHolders.
	
	self initializePrivateHooks.
		
	self initializePrivateAnnouncements.
]

{ #category : #initialization }
ComposablePresenter >> initializeDialogWindow: aWindow [
	"used to initialize the model in the case of the use into a dialog window"
]

{ #category : #initialization }
ComposablePresenter >> initializePresenter [
	"Used to specify the subwidgets, and/or to bind them together"
	"By default, do not do anything"
	
	extentHolder whenChangedDo: [:ex | 
		self widget ifNotNil: [:widget |
			(widget respondsTo: #extent:)
				ifTrue: [ widget extent: ex ]]].
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
	needRebuild := true asValueHolder.
	keyStrokesForNextFocusHolder := { KMNoShortcut new } asValueHolder.
	keyStrokesForPreviousFocusHolder := { KMNoShortcut new } asValueHolder.
	additionalKeyBindings := Dictionary new.
	aboutText := nil asValueHolder.
	windowIcon := nil asValueHolder.
	window := nil asValueHolder.
	askOkToClose := false asValueHolder.
	titleHolder := self class title asValueHolder.
	

]

{ #category : #initialization }
ComposablePresenter >> initializeWidgets [
	
	self subclassResponsibility
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
	
	^ self window notNil and: [ self window isDisplayed ]
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
ComposablePresenter >> needRebuild [

	^ needRebuild value
]

{ #category : #accessing }
ComposablePresenter >> needRebuild: aBoolean [

	needRebuild value: aBoolean
]

{ #category : #api }
ComposablePresenter >> neglectMenuModel: aMenuModel [

	aMenuModel menuGroups do: [ :group |
		group menuItems do: [ :item |
			item shortcut ifNotNil: [ :shortcut | self removeMenuKeyCombination: shortcut ].
			item subMenu ifNotNil: [ :subMenu | subMenu neglect: self	] ] ]
]

{ #category : #widgets }
ComposablePresenter >> newButton [
	^ self instantiate: ButtonPresenter
]

{ #category : #widgets }
ComposablePresenter >> newCheckBox [
	^ self instantiate: CheckBoxPresenter
]

{ #category : #widgets }
ComposablePresenter >> newCode [
	^ (self instantiate: TextPresenter)
		beForCode;
		yourself
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
ComposablePresenter >> newIconList [
	^ self instantiate: IconListPresenter
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
ComposablePresenter >> newList [
	^ self instantiate: FastTablePresenter
]

{ #category : #widgets }
ComposablePresenter >> newMultiColumnList [
	^ self instantiate: MultiColumnListPresenter
]

{ #category : #widgets }
ComposablePresenter >> newNullPresenter [
	^ self instantiate: NullPresenter
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
ComposablePresenter >> newTab [
	^ self instantiate: TabPresenter
]

{ #category : #widgets }
ComposablePresenter >> newTabManager [
	^ self instantiate: TabManagerPresenter
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
ComposablePresenter >> newTree [
	^ self instantiate: TreePresenter
]

{ #category : #private }
ComposablePresenter >> okToChange [

	^ owner 
		ifNil: [
			self canDiscardEdits ifTrue: [^ true].
			self changed: #wantToChange.  "Solicit cancel from view"
			^ self canDiscardEdits]
		ifNotNil: [ owner okToChange ]
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

{ #category : #api }
ComposablePresenter >> openDialogWithSpec [
	"Build the widget using the default spec and display it into a window"

	^ self openDialogWithSpec: self defaultSpecSelector.
]

{ #category : #api }
ComposablePresenter >> openDialogWithSpec: aSpec [
	"Build the widget using the spec name provided as argument and display it into a window"

	^ self openDialogWithSpecLayout: (self retrieveSpec: aSpec)
]

{ #category : #api }
ComposablePresenter >> openDialogWithSpecLayout: aSpec [
	"Build the widget using the spec name provided as argument and display it into a window"

	(window value notNil and: [ self needRebuild not ])
		ifTrue: [
			window value rebuildWithSpec: aSpec ]
		ifFalse: [
			window value: (DialogWindowPresenter new model: self).
			window value openWithSpecLayout: aSpec.
			self initializeDialogWindow: window value.
			window value updateTitle.
			self takeKeyboardFocus ].
		
	^ window value
]

{ #category : #api }
ComposablePresenter >> openWithSpec [
	"Build the widget using the default spec and display it into a window"

	^ self openWithSpec: self defaultSpecSelector.
]

{ #category : #api }
ComposablePresenter >> openWithSpec: aSpec [
	"Build the widget using the spec name provided as argument and display it into a window"

	^ self openWithSpecLayout: (self retrieveSpec: aSpec)
]

{ #category : #api }
ComposablePresenter >> openWithSpecLayout: aSpec [
	"Build the widget using the spec name provided as argument and display it into a window"

	(window value notNil and: [ self needRebuild not ])
		ifTrue: [
			window value rebuildWithSpecLayout: aSpec ]
		ifFalse: [ 
			window value: (self defaultWindowModelClass new model: self).
			window value openWithSpecLayout: aSpec.
			self takeKeyboardFocus ].
		
	^ window value
]

{ #category : #accessing }
ComposablePresenter >> owner [

	^ owner
]

{ #category : #accessing }
ComposablePresenter >> owner: anObject [

	owner := anObject.
]

{ #category : #private }
ComposablePresenter >> privateAdapterFromModel: aModel withSpec: aSpec [
	"apparently when looking at the implementation, it does not return a widget but an adapter so it should be called adapter :)"
	^ SpecInterpreter private_buildWidgetFor: self withSpec: aSpec.
]

{ #category : #private }
ComposablePresenter >> private_buildWithSpec [
	"Build the widget using the default spec"
	
	^ self private_buildWithSpec: self defaultSpecSelector
]

{ #category : #private }
ComposablePresenter >> private_buildWithSpec: aSpec [
	"Build the widget using the spec name provided as argument"
	| adapter |
	adapter := self privateAdapterFromModel: self withSpec: aSpec.
	self setExtentAndBindingTo: adapter widget.	
	^ adapter
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

	additionalKeyBindings 
		removeKey: aShortcut 
		ifAbsent: [ ^ self ].
	
	self changed: #removeKeyCombination: with: { aShortcut }
]

{ #category : #'api-shortcuts' }
ComposablePresenter >> removeMenuKeyCombination: aShortcut [

	additionalKeyBindings 
		removeKey: aShortcut 
		ifAbsent: [ ^ self ].
	
	self changed: #removeMenuKeyCombination: with: { aShortcut }
]

{ #category : #'instance creation' }
ComposablePresenter >> resolveSymbol: aSymbol [

	^ Smalltalk at: aSymbol
]

{ #category : #api }
ComposablePresenter >> retrieveSpec: aSelector [
	| layout |
	
	layout := self class perform: aSelector.
	layout isSpecLayout ifTrue: [ layout selector: aSelector ].
	^ layout
]

{ #category : #api }
ComposablePresenter >> setExtentAndBindingTo: widget [

	self ensureExtentFor: widget.
	self ensureKeyBindingsFor: widget.
	self announce: (WidgetBuilt model: self widget: widget).
	
]

{ #category : #api }
ComposablePresenter >> setModal: aWindow [ 
	
	self changed: #setModal: with: { aWindow }
]

{ #category : #'model access' }
ComposablePresenter >> setModel: aDomainObject [


]

{ #category : #'model access' }
ComposablePresenter >> setModelBeforeInitialization: aDomainObject [


]

{ #category : #api }
ComposablePresenter >> show [

	self widget ifNotNil: [:widget |
		(widget respondsTo: #show) ifTrue: [ widget show ]].
]

{ #category : #accessing }
ComposablePresenter >> spec [

	^ spec
]

{ #category : #accessing }
ComposablePresenter >> spec: aSpec [

	spec := aSpec
]

{ #category : #api }
ComposablePresenter >> specSelectors [
	"Return all the spec names"
	
	^ self class specSelectors
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

{ #category : #api }
ComposablePresenter >> title [
	"Return the window's title"

	^ titleHolder value
]

{ #category : #api }
ComposablePresenter >> title: aString [

	titleHolder value: aString
]

{ #category : #private }
ComposablePresenter >> update: aParameter [

	self changed: aParameter
]

{ #category : #initialization }
ComposablePresenter >> updatePresenter [
]

{ #category : #api }
ComposablePresenter >> updateTitle [
	"Update the window title"

	self window ifNotNil: [:w | w updateTitle ]
]

{ #category : #'api-events' }
ComposablePresenter >> whenBuiltDo: aBlock [

	self announcer 
		when: WidgetBuilt
		do: aBlock
]

{ #category : #'api-events' }
ComposablePresenter >> whenShortcutsChanged: aBlock [
	"Set a block to value when the shortcuts block has changed"
	additionalKeyBindings whenChangedDo: aBlock
]

{ #category : #'api-events' }
ComposablePresenter >> whenWindowChanged: aBlock [

	window whenChangedDo: aBlock
]

{ #category : #accessing }
ComposablePresenter >> widget [

	^ spec
		ifNil: [ nil ]
		ifNotNil: [:s | s instance ]
]

{ #category : #accessing }
ComposablePresenter >> window [

	^ window value
		ifNil: [
			owner
				ifNil: [ nil ]
				ifNotNil: [:o | o window ]]
]

{ #category : #api }
ComposablePresenter >> windowIcon [

	^ windowIcon value
]

{ #category : #api }
ComposablePresenter >> windowIcon: aForm [

	windowIcon value: aForm
]

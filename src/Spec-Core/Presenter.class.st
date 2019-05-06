"
I'm a base for all presenters. 
I define common behaviours for widget presenters and also for composable presenters (the ones an user regularly uses).
"
Class {
	#name : #Presenter,
	#superclass : #Model,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'owner',
		'adapter',
		'needRebuild'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #TOREMOVE }
Presenter class >> buttonHeight [

	^ self defaultFont height + 12
]

{ #category : #TOREMOVE }
Presenter class >> buttonWidth [

	^ (self defaultFont widthOfString: 'eilwp') + 44
]

{ #category : #TOREMOVE }
Presenter class >> defaultFont [ 

	self flag: #TODO. "This is so wrong. All this needs to be in the theme and in the 
	application"
	^ StandardFonts defaultFont
]

{ #category : #TOREMOVE }
Presenter class >> inputTextHeight [

	^ self defaultFont height + 12
]

{ #category : #TOREMOVE }
Presenter class >> labelHeight [

	^ self defaultFont height
]

{ #category : #'instance creation' }
Presenter class >> owner: anOwningPresenter [

	^ self basicNew
		owner: anOwningPresenter;
		initialize;
		yourself
]

{ #category : #accessing }
Presenter class >> toolName [
	"The tool name can be used in some places such as the About window's title."
	
	^ self name
]

{ #category : #TOREMOVE }
Presenter class >> toolbarHeight [

	^ self defaultFont height + 12
]

{ #category : #accessing }
Presenter >> adapter [

	^ adapter
]

{ #category : #accessing }
Presenter >> adapter: anAdapter [

	adapter := anAdapter
]

{ #category : #accessing }
Presenter >> application [
	"Answer application owner of this composition.
	 The tree for a composition starts always with an application, which is responsible of 
	 providing all resources the presenter needs. "

	^ self owner application
]

{ #category : #'private building' }
Presenter >> basicBuildAdapterWithSpecLayout: aSpecLayout [
	"I assume the SpecBindings is well defined"
	
	self adapter: (aSpecLayout asSpecLayout 
		buildAdapterFor: self 
		bindings: SpecBindings value).

	self announcer announce: (WidgetBuilt 
		model: self 
		widget: adapter widget).
	
	^ adapter
]

{ #category : #'private building' }
Presenter >> buildAdapterWithSpecLayout: aSpecLayout [
	"Build the adapter using the spec name provided as argument"
	
	^ SpecBindings 
		value: self application adapterBindings
		during: [ self basicBuildAdapterWithSpecLayout: aSpecLayout ]
]

{ #category : #building }
Presenter >> buildWithSpec [
	"Build the widget using the default spec"
	
	^ self buildWithSpecLayout: self retrieveDefaultSpec
]

{ #category : #building }
Presenter >> buildWithSpec: aSpec [
	"Build the widget using the spec name provided as argument"
	
	^ self buildWithSpecLayout: (self retrieveSpec: aSpec)
]

{ #category : #building }
Presenter >> buildWithSpecLayout: aSpecLayout [
	"Build the widget using the spec name provided as argument"

	^ (self buildAdapterWithSpecLayout: aSpecLayout) widget
]

{ #category : #'private building' }
Presenter >> defaultSpecSelector [
	| selectors |
	
	selectors := 
		(Pragma allNamed: #spec: from: self class class  to: Presenter class class),
		(Pragma allNamed: #spec from: self class class to: Presenter class class).
	
	selectors ifEmpty: [ ^ #defaultSpec ].	
	selectors size = 1 ifTrue: [ ^ selectors first methodSelector ].
	^ (selectors detect: [ :each | (each argumentAt: 1) = #default ]) methodSelector
	

]

{ #category : #accessing }
Presenter >> enabled: aBoolean [ 

	^ self subclassResponsibility
]

{ #category : #testing }
Presenter >> hasOwner [

	^ self owner notNil
]

{ #category : #testing }
Presenter >> hasWindow [

	^ self root isWindowPresenter
]

{ #category : #initialization }
Presenter >> initialize [ 

	super initialize.
	needRebuild := true asValueHolder
]

{ #category : #initialization }
Presenter >> initializeDialogWindow: aDialogWindowPresenter [
	"used to initialize the model in the case of the use into a dialog window.
	 Override this to set buttons other than the default (Ok, Cancel)."
	
	aDialogWindowPresenter
		addButton: 'Ok' do: [ :presenter | 
			presenter triggerOkAction.
			presenter close ];
		addButton: 'Cancel' do: [ :presenter | 
			presenter triggerCancelAction.
			presenter close ]	
]

{ #category : #initialization }
Presenter >> initializeWindow: aWindowPresenter [
	"override this to set window values before opening. 
	 You may want to add a menu, a toolbar or a statusbar"

	self flag: #TODO. "Does presenters needs to understand this? (they do if we 
	want to do ButtonPresenter new openWithSpec)."
]

{ #category : #testing }
Presenter >> isMenuPresenter [

	^ false
]

{ #category : #testing }
Presenter >> isPresenter [

	^ true
]

{ #category : #testing }
Presenter >> isWindowPresenter [

	^ false
]

{ #category : #accessing }
Presenter >> needRebuild [

	^ needRebuild value
]

{ #category : #accessing }
Presenter >> needRebuild: aBoolean [

	needRebuild value: aBoolean
]

{ #category : #showing }
Presenter >> openDialogWithSpec [
	"Build the widget using the default spec and display it into a dialog. 
	 Beware: this way will not create a real modal in the sense that the user will keep the focus on other windows. If you want to make the user focus on your dialog only you should use #openModalWithSpec instead."
	
	^ self application 
		openDialog: self
		withSpecLayout: self retrieveDefaultSpec
]

{ #category : #showing }
Presenter >> openDialogWithSpec: aSelector [
	"Build the widget using the default spec and display it into a dialog. 
	 Beware: this way will not create a real modal in the sens that the user will keep the focus on other windows. If you want to make the user focus on your dialog only you should use #openModalWithSpec: instead."
	
	^ self application 
		openDialog: self
		withSpecLayout: (self retrieveSpec: aSelector)
]

{ #category : #showing }
Presenter >> openDialogWithSpecLayout: aSpecLayout [

	^ self application 
		openDialog: self
		withSpecLayout: aSpecLayout
]

{ #category : #showing }
Presenter >> openModalWithSpec [

	^ self application 
		openModal: self
		withSpecLayout: self retrieveDefaultSpec
]

{ #category : #showing }
Presenter >> openModalWithSpec: aSelector [

	^ self application 
		openModal: self
		withSpecLayout: (self retrieveSpec: aSelector)
]

{ #category : #showing }
Presenter >> openModalWithSpecLayout: aSpecLayout [

	^ self application 
		openModal: self
		withSpecLayout: aSpecLayout
]

{ #category : #showing }
Presenter >> openWithSpec [

	^ self application 
		open: self
		withSpecLayout: self retrieveDefaultSpec
]

{ #category : #showing }
Presenter >> openWithSpec: aSelector [

	^ self application 
		open: self
		withSpecLayout: (self retrieveSpec: aSelector)
]

{ #category : #showing }
Presenter >> openWithSpecLayout: aSpecLayout [

	^ self application 
		open: self
		withSpecLayout: aSpecLayout
]

{ #category : #accessing }
Presenter >> owner [

	^ owner
]

{ #category : #accessing }
Presenter >> owner: aPresenter [

	owner := aPresenter
]

{ #category : #'private building' }
Presenter >> retrieveDefaultSpec [

	^ self retrieveSpec: self defaultSpecSelector
]

{ #category : #'private building' }
Presenter >> retrieveSpec: aSelector [
	| layout |

	layout := self class perform: aSelector.
	layout isSpecLayout ifTrue: [ 
		layout selector: aSelector ].
	^ layout
]

{ #category : #accessing }
Presenter >> root [
	"Answer root presenter of this composition."
	
	^ self owner 
		ifNotNil: [ :presenter | presenter root ]
		ifNil: [ self ]
]

{ #category : #accessing }
Presenter >> spec [
	self flag: #TODO. "This needs to be removed, but there is a problem with the interpreter.
	(it will be solved as soon as the interpreter is removed :P)"
	
	^ self adapter
]

{ #category : #accessing }
Presenter >> spec: anAdapter [
	"This is a remaining of other times... spec accesor in fact contains an adapter"
	
	self flag: #TODO. "Same as #spec, it needs to go"
	self adapter: anAdapter
]

{ #category : #'accessing ports' }
Presenter >> transmitTo: aPresenter [		
	
	^ self owner transmit 
		from: self to: aPresenter;
		yourself
]

{ #category : #'accessing ports' }
Presenter >> transmitTo: aPresenter transform: aValuable [
	
	^ self owner transmit 
		from: self to: aPresenter;
		transform: aValuable;
		yourself
]

{ #category : #'accessing ports' }
Presenter >> transmitTo: aPresenter transform: aValuable postTransmission: postValuable [
	
	^ self owner transmit 
		from: self to: aPresenter;
		transform: aValuable;
		postTransmission: postValuable;
		yourself
]

{ #category : #updating }
Presenter >> update: aParameter [

	self changed: aParameter
]

{ #category : #events }
Presenter >> whenBuiltDo: aBlock [

	self announcer 
		when: WidgetBuilt
		do: aBlock
]

{ #category : #accessing }
Presenter >> window [
	"Answer window containing this composition."
	
	^ self hasWindow
		ifTrue: [ self root ]
		ifFalse: [ nil ]
]

{ #category : #'private utils' }
Presenter >> withAdapterDo: aValuable [
	"a convenience method to avoid verify by nil all the time"
	
	^ self adapter ifNotNil: aValuable
]

{ #category : #'private utils' }
Presenter >> withWidgetDo: aValuable [
	"a convenience method to avoid verify by nil all the time"

	^ self adapter ifNotNil: aValuable
]

{ #category : #'private utils' }
Presenter >> withWindowDo: aValuable [

	self hasWindow ifFalse: [ ^ self ].
	"Since Presenter has window, root = window"
	aValuable value: self root
]

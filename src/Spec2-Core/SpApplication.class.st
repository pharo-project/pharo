"
I'm a Spec application entry point. 
In Spec, each application needs to have a definition that concentrates its flow and resources.

! SpApplication
A SpApplication is a class that handles many aspectes of a Spec Application (hence it's name) in a convenient fashion. 
SpApplication handles your application initialisation, configuration and resources. It also keeps the windows you have currently opened. 

!! Initialisation
Initialisation of an application includes (non mandatory): configure the backend you want to use, add useful resources and define a start method that will call your initial window.

!!! Configure backend
Spec2 includes several backens (for the moment, Morphic and Gtk). A SpApplication configures a Morphic backend by default, but you can change it using ==#useBackend:== or ==#useBackend:with:== and sending the backend identifier and optionally a configuration (you may want to do specific backend things to configure your application behavior).

@NOTE: Is very important that after you initiate your application, you ==run== it.

Examples:
> self example1.
> self example2.

see also: *SpApplicationConfiguration*

!!! Add resources
During initialisation, you may want to add special resources (like icons, themes, etc.).
While you can add your own way to access resources, SpApplication provides a property registration mechanism (a simple Dictionary and accessors), you may find useful to search at ==accessing properties== protocol.

!!! Defining a start method.
This is useful to give your application a starting window (in general, this is what you want).

Example: 
[[[lang=Smalltalk
MyApplication>>start

   (self new: MyMainPresenter) openWithSpec
]]]
"
Class {
	#name : #SpApplication,
	#superclass : #Object,
	#instVars : [
		'backend',
		'windows',
		'properties',
		'configuration'
	],
	#classVars : [
		'DefaultApplication'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #accessing }
SpApplication class >> defaultApplication [

	^ DefaultApplication ifNil: [ DefaultApplication := SpNullApplication new ]
]

{ #category : #accessing }
SpApplication class >> defaultBackendName [

	^ #Morphic
]

{ #category : #examples }
SpApplication class >> example1 [
	"This example shows how to change the backend of an application" 
	| app |
	
	"You want to subclass SpApplication to create your app"
	app := SpApplication new.
	app useBackend: #Gtk.
	app run
]

{ #category : #examples }
SpApplication class >> example2 [
	"This example shows how to change the backend of an application" 
	| app |
	
	"You want to subclass SpApplication to create your app"
	app := SpApplication new.
	"You want to subclass SpGtkConfiguration to create your own cofiguration"
	app useBackend: #Gtk with: SpGtkConfiguration new.
	app run
]

{ #category : #accessing }
SpApplication >> adapterBindings [
	
	^ self backend adapterBindings
]

{ #category : #'accessing backend' }
SpApplication >> backend [

	^ backend ifNil: [ 
		self useBackend: self class defaultBackendName.
		backend ]
]

{ #category : #showing }
SpApplication >> close [
	
	self windows copy do: #close
]

{ #category : #'accessing backend' }
SpApplication >> configuration [

	^ configuration
]

{ #category : #accessing }
SpApplication >> defer: aBlock [

	self backend defer: aBlock
]

{ #category : #windows }
SpApplication >> hasWindow: aWindow [
	
	^ self windows includes: aWindow
]

{ #category : #testing }
SpApplication >> isPresenter [ 
	"Presenters use this to track application/rootPresenter."

	^ false
]

{ #category : #factory }
SpApplication >> new: aPresenter [
	"a synonym for #newPresenter:"
	
	^ self newPresenter: aPresenter
]

{ #category : #factory }
SpApplication >> newPresenter: aPresenterClass [

	^ aPresenterClass newApplication: self
]

{ #category : #'ui notifying' }
SpApplication >> notify: aSpecNotification [
	"how notifications are handled depends on the backend"

	aSpecNotification dispatchTo: self backend
]

{ #category : #showing }
SpApplication >> open: aPresenter [

	^ self 
		open: aPresenter 
		withSpecLayout: aPresenter defaultSpec
		using: aPresenter defaultWindowPresenterClass
]

{ #category : #showing }
SpApplication >> open: aPresenter withSpecLayout: aSpecLayout [

	^ self 
		open: aPresenter 
		withSpecLayout: aSpecLayout 
		using: aPresenter defaultWindowPresenterClass
]

{ #category : #'private showing' }
SpApplication >> open: aPresenter withSpecLayout: aSpecLayout using: aWindowClass [
	| window |

	self validateApplicationOfPresenter: aPresenter.

	window := aWindowClass presenter: aPresenter.
	self windows add: window.
	window openWithSpecLayout: aSpecLayout.
	
	^ window
]

{ #category : #showing }
SpApplication >> openDialog: aPresenter [ 
	
	^ self 
		openDialog: aPresenter 
		withSpecLayout: aPresenter defaultSpec

]

{ #category : #showing }
SpApplication >> openDialog: aPresenter withSpecLayout: aSpecLayout [

	^ self 
		open: aPresenter 
		withSpecLayout: aSpecLayout 
		using: SpDialogWindowPresenter
]

{ #category : #showing }
SpApplication >> openModal: aPresenter [ 
	
	^ self 
		openModal: aPresenter 
		withSpecLayout: aPresenter defaultSpec

]

{ #category : #showing }
SpApplication >> openModal: aPresenter withSpecLayout: aSpecLayout [

	^ self 
		open: aPresenter 
		withSpecLayout: aSpecLayout 
		using: SpModalWindowPresenter
]

{ #category : #'accessing properties' }
SpApplication >> properties [

	^ properties ifNil: [ properties := SmallDictionary new ]
]

{ #category : #'accessing properties' }
SpApplication >> propertyAt: aKey [

	^ self properties at: aKey
]

{ #category : #'accessing properties' }
SpApplication >> propertyAt: aKey ifAbsent: aBlock [

	^ self properties 
		at: aKey
		ifAbsent: aBlock
]

{ #category : #'accessing properties' }
SpApplication >> propertyAt: aKey ifAbsentPut: aBlock [

	^ self properties 
		at: aKey
		ifAbsentPut: aBlock
]

{ #category : #'accessing properties' }
SpApplication >> propertyAt: aKey ifPresent: aBlock [

	^ self properties
		at: aKey 
		ifPresent: aBlock
]

{ #category : #'accessing properties' }
SpApplication >> propertyAt: aKey ifPresent: aBlock ifAbsent: absentBlock [

	^ self properties
		at: aKey 
		ifPresent: aBlock
		ifAbsent: absentBlock
]

{ #category : #'accessing properties' }
SpApplication >> propertyAt: aKey ifPresent: aBlock ifAbsentPut: absentBlock [

	^ self properties
		at: aKey 
		ifPresent: aBlock
		ifAbsentPut: absentBlock
]

{ #category : #'accessing properties' }
SpApplication >> propertyAt: aKey put: aValue [

	^ self properties 
		at: aKey
		put: aValue
]

{ #category : #running }
SpApplication >> run [ 

	self start
]

{ #category : #'ui dialogs' }
SpApplication >> selectDirectoryTitle: aString [

	^ self backend selectDirectoryTitle: aString
]

{ #category : #'ui dialogs' }
SpApplication >> selectFileTitle: aString [

	^ self backend selectFileTitle: aString
]

{ #category : #running }
SpApplication >> start [ 
	"Override this to start your app"
]

{ #category : #'accessing backend' }
SpApplication >> useBackend: aName [

	backend := SpApplicationBackend findBackendNamed: aName
]

{ #category : #'accessing backend' }
SpApplication >> useBackend: aName with: aConfiguration [

	self useBackend: aName.
	configuration := aConfiguration.
	aConfiguration configure: self
]

{ #category : #'private showing' }
SpApplication >> validateApplicationOfPresenter: aPresenter [

	aPresenter application == self ifFalse: [ 
		SpInvalidApplicationError signal: 'The requested presenter has a different application' ]
]

{ #category : #windows }
SpApplication >> windowClosed: aWindowPresenter [
	
	self windows remove: aWindowPresenter ifAbsent: [  ]
]

{ #category : #accessing }
SpApplication >> windows [
	
	^ windows ifNil: [ windows := Set new ]
]

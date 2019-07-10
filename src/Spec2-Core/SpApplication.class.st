"
I'm a Spec application entry point. 
In Spec, each application needs to have a definition that concentrates its flow and resources.

"
Class {
	#name : #SpApplication,
	#superclass : #Object,
	#instVars : [
		'backend',
		'windows'
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

{ #category : #accessing }
SpApplication >> adapterBindings [
	
	^ self backend adapterBindings
]

{ #category : #accessing }
SpApplication >> backend [

	^ backend ifNil: [ 
		self useBackend: self class defaultBackendName.
		backend ]
]

{ #category : #showing }
SpApplication >> close [
	
	self windows copy do: #close
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

{ #category : #accessing }
SpApplication >> useBackend: aName [

	backend := SpApplicationBackend findBackendNamed: aName
]

{ #category : #accessing }
SpApplication >> useBackend: aName with: aConfiguration [

	self useBackend: aName.
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

"
I'm a CompsableModel with a dynamic behavior for managing subwidgets and the value holder communicates with them.

The idea is that instead of having a fix set of valueHolders controlling various aspects of the model. I have a dictionary to hold such elements.

You can also have a look at DynamicalPopup for an example.

I provide the following variables and its accessors
- layout to put a layout created dynamically.
- widget is a Dictionary containing my dynamics widgets.

I'm specializing
- doesNotUnderstand:  to search in my dictionary before rise an exception, so that the programmer can simply use accessors to access my dynamic elements. 
- openWithSpec to use the layout in my variable if it is not nil.

I provide the following messages
- instantiateModels: to create instances of subwidgets. It takes a collection of pair, first of each pair is the string to use as key in the dictionary, second is the class name to instantiate.
- assign:to: to put in the dictionary a widget already instantiated.

todo
- needFullRebuild:
- retrieveSpec:
"
Class {
	#name : #SpDynamicPresenter,
	#superclass : #SpPresenter,
	#traits : 'SpTDynamicPresenter',
	#classTraits : 'SpTDynamicPresenter classTrait',
	#category : #'Spec2-Core-Base'
}

{ #category : #'instance creation' }
SpDynamicPresenter >> assign: aPresenter to: anInstVarName [
	self presenterAt: anInstVarName put: aPresenter.
	aPresenter owner: self
]

{ #category : #'error handling' }
SpDynamicPresenter >> doesNotUnderstand: aMessage [
	"To make sure that we can use accessors to access the dynamically managed subcomponents."

	^ self presenterAt: aMessage selector ifAbsent: [ super doesNotUnderstand: aMessage ]
]

{ #category : #initialization }
SpDynamicPresenter >> initializePresenters [
]

{ #category : #'instance creation' }
SpDynamicPresenter >> instantiatePresenters: aCollectionOfPairs [
	aCollectionOfPairs pairsDo: [ :k :v | self presenterAt: k asSymbol put: (self createInstanceFor: v) ]
]

{ #category : #accessing }
SpDynamicPresenter >> layout [
	^ layout
]

{ #category : #accessing }
SpDynamicPresenter >> layout: aLayout [
	layout := aLayout
]

{ #category : #api }
SpDynamicPresenter >> needFullRebuild: aBoolean [

	self needRebuild: aBoolean.
	self presentersDo: [ :w | w needRebuild: aBoolean ]
]

{ #category : #api }
SpDynamicPresenter >> openModalWithSpec [

	self layout ifNil: [ ^ super openWithSpec ].
	^ self application 
		openModal: self
		withSpecLayout: self layout
]

{ #category : #api }
SpDynamicPresenter >> openWithSpec [

	self layout ifNil: [ ^ super openWithSpec ].
	^ self application 
		open: self
		withSpecLayout: self layout
]

{ #category : #api }
SpDynamicPresenter >> retrieveSpec: aSelector [

	self layout ifNil: [ ^ super retrieveSpec: aSelector ].

	^ self layout
]

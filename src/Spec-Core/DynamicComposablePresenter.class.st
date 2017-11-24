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
	#name : #DynamicComposablePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'widgets',
		'layout'
	],
	#category : #'Spec-Core'
}

{ #category : #'instance creation' }
DynamicComposablePresenter >> assign: aModel to: anInstVarName [

	widgets 
		at: anInstVarName
		put: aModel.
		
	aModel owner: self
]

{ #category : #'error handling' }
DynamicComposablePresenter >> doesNotUnderstand: aMessage [
	"To make sure that we can use accessors to access the dynamically managed subcomponents."
	
	^ widgets 
			at: aMessage selector 
			ifAbsent: [ super doesNotUnderstand: aMessage ]
]

{ #category : #initialization }
DynamicComposablePresenter >> initialize [

	widgets := OrderedIdentityDictionary new asValueHolder.
	layout := nil asValueHolder.
	super initialize.	
]

{ #category : #initialization }
DynamicComposablePresenter >> initializeWidgets [
]

{ #category : #'instance creation' }
DynamicComposablePresenter >> instantiateModels: aCollectionOfPairs [
 
	aCollectionOfPairs pairsDo: [ :k :v |
		widgets at: k asSymbol put: (self createInstanceFor: v) ]
]

{ #category : #'instance creation' }
DynamicComposablePresenter >> instantiatePresenters: aCollectionOfPairs [
 
	^ self instantiateModels: aCollectionOfPairs
]

{ #category : #accessing }
DynamicComposablePresenter >> layout [

	^ layout value
]

{ #category : #accessing }
DynamicComposablePresenter >> layout: aLayout [

	layout value: aLayout
]

{ #category : #api }
DynamicComposablePresenter >> needFullRebuild: aBoolean [

	self needRebuild: aBoolean.
	self widgetsDo: [:e | e needRebuild: aBoolean ]
]

{ #category : #api }
DynamicComposablePresenter >> openWithSpec [

	self layout ifNil: [ ^ super openWithSpec ].
	^ self openWithSpecLayout: self layout
]

{ #category : #api }
DynamicComposablePresenter >> retrieveSpec: aSelector [

	self layout ifNil: [ ^ super retrieveSpec: aSelector ].

	^ self layout
		selector: aSelector;
		yourself
]

{ #category : #accessing }
DynamicComposablePresenter >> widgets [

	^ widgets
]

{ #category : #accessing }
DynamicComposablePresenter >> widgetsDo: aBlock [

	self widgets do: aBlock 
]

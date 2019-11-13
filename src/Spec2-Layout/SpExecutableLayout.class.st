Class {
	#name : #SpExecutableLayout,
	#superclass : #Object,
	#instVars : [
		'children',
		'selector',
		'adapter'
	],
	#category : #'Spec2-Layout-Base'
}

{ #category : #private }
SpExecutableLayout >> adapterName [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpExecutableLayout >> add: aName withConstraints: aBlock [
	| constraints |

	constraints := self constraintsClass new.
	aBlock value: constraints.
	children at: aName put: constraints
]

{ #category : #converting }
SpExecutableLayout >> asArray [

	self error: 'Should not arrive here. This layout is executable then it will not be interpreted.'
]

{ #category : #converting }
SpExecutableLayout >> asSpLayout [
	
	^ self
]

{ #category : #building }
SpExecutableLayout >> buildAdapterFor: aPresenter bindings: bindings [

	adapter := (aPresenter needRebuild or: [ aPresenter adapter isNil ])
		ifTrue: [ 
			(bindings adapterClass: self adapterName) adapt: aPresenter ]
		ifFalse: [ 
			aPresenter adapter
				removeSubWidgets;
				yourself ].
			
	"Ensure we remove the flag `no need of rebuild` if it was set."
	aPresenter needRebuild: true.
	
	adapter layout: self.

	children keysAndValuesDo: [ :presenterNameOrLayout :constraints | 
		"Since a layout can be composed (by other layouts), we can include a 
		 presenter or a layout" 
		adapter 
			add: (self 
				resolvePresenter: presenterNameOrLayout 
				presenter: aPresenter 
				bindings: bindings) 
			constraints: constraints ].

	^ adapter
]

{ #category : #accessing }
SpExecutableLayout >> children [

	^ children
]

{ #category : #private }
SpExecutableLayout >> constraintsClass [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpExecutableLayout >> deepPresenters [
	"answer all presenters in the layout hierarchy"
	
	^ self presenters
		inject: #()
		into: [ :all :each | 
			all, (each isSpLayout 
				ifTrue: [ each deepPresenters ]
				ifFalse: [ { each } ]) ]
]

{ #category : #initialization }
SpExecutableLayout >> initialize [ 

	super initialize.
	children := OrderedDictionary new
]

{ #category : #testing }
SpExecutableLayout >> isExecutable [

	^ true
]

{ #category : #testing }
SpExecutableLayout >> isSpLayout [

	^ true
]

{ #category : #accessing }
SpExecutableLayout >> presenters [
	
	^ children keys
]

{ #category : #private }
SpExecutableLayout >> resolvePresenter: presenterNameOrLayout presenter: aPresenter bindings: bindings [

	"most common case: I receive a symbol that I need to convert into a presenter"
	presenterNameOrLayout isSymbol ifTrue: [ 
		^ (self subpresenterOrLayoutNamed: presenterNameOrLayout of: aPresenter)
			ifNil: [ self error: 'You presenter named "', presenterNameOrLayout , '" from ', aPresenter printString ,' was not initialized.' ] ].
	
	"I receive a layout: dig inside."
	presenterNameOrLayout isSpLayout ifTrue: [ 
		^ presenterNameOrLayout buildAdapterFor: aPresenter bindings: bindings ].
	
	"I receive an arbitrary object (needs to understand #asPresenter)"
	^ presenterNameOrLayout asPresenter
		owner: aPresenter;
		yourself
]

{ #category : #accessing }
SpExecutableLayout >> selector [
	^ selector
]

{ #category : #accessing }
SpExecutableLayout >> selector: anObject [
	selector := anObject
]

{ #category : #private }
SpExecutableLayout >> subpresenterOrLayoutNamed: presenterNameOrLayout of: aPresenter [
	^ aPresenter additionalSubpresentersMap
		at: presenterNameOrLayout
		ifAbsent: [ aPresenter presenterAt: presenterNameOrLayout ifAbsent: [ self error: 'No presenter named "' , presenterNameOrLayout , '" was found in ' , aPresenter printString ] ]
]

{ #category : #accessing }
SpExecutableLayout >> traversePresentersDo: aBlock [

	self 
		traversePresentersDo: aBlock 
		excluding: Set new
]

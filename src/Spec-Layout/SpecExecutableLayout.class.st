Class {
	#name : #SpecExecutableLayout,
	#superclass : #Object,
	#instVars : [
		'children',
		'selector',
		'adapter'
	],
	#category : #'Spec-Layout-Base'
}

{ #category : #private }
SpecExecutableLayout >> adapterName [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpecExecutableLayout >> add: aName withConstraints: aBlock [
	| constraints |
	constraints := self constraintsClass new.
	aBlock value: constraints.
	children at: aName put: constraints
]

{ #category : #converting }
SpecExecutableLayout >> asArray [

	self error: 'Should not arrive here. This layout is executable then it will not be interpreted.'
]

{ #category : #converting }
SpecExecutableLayout >> asSpecLayout [
	
	^ self
]

{ #category : #building }
SpecExecutableLayout >> buildAdapterFor: aPresenter bindings: bindings [
	adapter := (aPresenter needRebuild or: [ aPresenter adapter isNil ])
		ifTrue: [ (bindings adapterClass: self adapterName) adapt: aPresenter ]
		ifFalse: [ aPresenter adapter
				removeSubWidgets;
				yourself ].
			
	"Ensure we remove the flag `no need of rebuild` if it was set."
	aPresenter needRebuild: true.

	adapter layout: self.

	children
		keysAndValuesDo:
			[ :presenterNameOrLayout :constraints | "Since a layout can be composed (by other layouts), we can include a presenter or a layout" adapter add: (self resolvePresenter: presenterNameOrLayout model: aPresenter bindings: bindings) constraints: constraints ].

	^ adapter
]

{ #category : #private }
SpecExecutableLayout >> constraintsClass [

	^ self subclassResponsibility
]

{ #category : #initialization }
SpecExecutableLayout >> initialize [ 

	super initialize.
	children := OrderedDictionary new
]

{ #category : #testing }
SpecExecutableLayout >> isExecutable [

	^ true
]

{ #category : #testing }
SpecExecutableLayout >> isSpecLayout [

	^ true
]

{ #category : #accessing }
SpecExecutableLayout >> presenters [
	
	^ children keys
]

{ #category : #private }
SpecExecutableLayout >> resolvePresenter: presenterNameOrLayout model: aModel bindings: bindings [

	"most common case: I receive a symbol that I need to convert into a presenter"
	presenterNameOrLayout isSymbol ifTrue: [ 
		^ (self subpresenterOrLayoutNamed: presenterNameOrLayout of: aModel)
			ifNil: [ self error: 'You presenter named "', presenterNameOrLayout , '" from ', aModel printString ,' was not initialized.' ] ].
	
	"I receive a layout: dig inside."
	presenterNameOrLayout isSpecLayout ifTrue: [ 
		^ presenterNameOrLayout buildAdapterFor: aModel bindings: bindings ].
	
	"I receive an arbitrary object (needs to understand #asPresenter)"
	^ presenterNameOrLayout asPresenter
		owner: aModel;
		yourself
]

{ #category : #accessing }
SpecExecutableLayout >> selector [
	^ selector
]

{ #category : #accessing }
SpecExecutableLayout >> selector: anObject [
	selector := anObject
]

{ #category : #private }
SpecExecutableLayout >> subpresenterOrLayoutNamed: presenterNameOrLayout of: aPresenter [
	^ aPresenter additionalSubpresentersMap
		at: presenterNameOrLayout
		ifAbsent: [ aPresenter presenterAt: presenterNameOrLayout ifAbsent: [ self error: 'No presenter named "' , presenterNameOrLayout , '" was found in ' , aPresenter printString ] ]
]

"
I am the adapter used to bridge a WindowPresenter and a SystemWindow
"
Class {
	#name : #MorphicWindowAdapter,
	#superclass : #AbstractMorphicAdapter,
	#instVars : [
		'isClosedHolder'
	],
	#category : #Spec-MorphicAdapters
}

{ #category : #'widget API' }
MorphicWindowAdapter >> aboutText [

	^ self model model
		ifNotNil: [ :m | m aboutText ]
]

{ #category : #'widget API' }
MorphicWindowAdapter >> aboutTitle [
    ^ self model model
         ifNotNil: [ :m | (m respondsTo: #aboutTitle)
		ifTrue: [m aboutTitle]
		ifFalse: [m class name]].
]

{ #category : #protocol }
MorphicWindowAdapter >> addModelIn: widgetToBuild withSpec: aSpec [
	
	widgetToBuild
		addMorph: (self model model buildWithSpec: aSpec)
		frame: (0@0 corner: 1@1)
]

{ #category : #protocol }
MorphicWindowAdapter >> addModelIn: widgetToBuild withSpecLayout: aSpec [
	
	widgetToBuild
		addMorph: (self model model buildWithSpecLayout: aSpec)
		frame: (0@0 corner: 1@1)
]

{ #category : #'widget API' }
MorphicWindowAdapter >> addModelItemsToWindowMenu: aMenu [

	self model addMenuItemsToWindowMenu: aMenu
]

{ #category : #'widget API' }
MorphicWindowAdapter >> askOkToClose [

	^ self model askOkToClose
]

{ #category : #factory }
MorphicWindowAdapter >> buildWidget [
	^ SpecWindow new
		model: model;
		isResizeable: self isResizeable;
		yourself
]

{ #category : #'widget API' }
MorphicWindowAdapter >> cancelled [

	^ false
]

{ #category : #protocol }
MorphicWindowAdapter >> centered [

	self widgetDo: [ :w | 
		w
			fullBounds;
			position: Display extent - w extent // 2 ]
]

{ #category : #protocol }
MorphicWindowAdapter >> centeredRelativeTo: aModel [

	self widgetDo: [ :w | aModel centerWidget: w ]
]

{ #category : #protocol }
MorphicWindowAdapter >> close [

	self widget ifNotNil: [ :w | w close ].
]

{ #category : #protocol }
MorphicWindowAdapter >> extent: aPoint [

	^ self widget extent: aPoint
]

{ #category : #'widget API' }
MorphicWindowAdapter >> initialExtent [

	^ self model initialExtent
]

{ #category : #initialize }
MorphicWindowAdapter >> initialize [

	super initialize.

	isClosedHolder := false asValueHolder.
]

{ #category : #protocol }
MorphicWindowAdapter >> isDisplayed [

	^ self widget isDisplayed
]

{ #category : #'widget API' }
MorphicWindowAdapter >> isResizeable [

	^ self model isResizeable
]

{ #category : #'spec protocol' }
MorphicWindowAdapter >> isResizeable: aBoolean [

	^ self widgetDo: [ :w | w isResizeable: aBoolean ]
]

{ #category : #'spec protocol' }
MorphicWindowAdapter >> maximize [

	self widgetDo: [ :w | w maximize ]
]

{ #category : #'spec protocol' }
MorphicWindowAdapter >> minimize [

	self widgetDo: [ :w | w minimize ]
]

{ #category : #'spec protocol' }
MorphicWindowAdapter >> modalRelativeTo: aWindow [

	self widgetDo: [ :w | aWindow setModal: w ]
]

{ #category : #'widget API' }
MorphicWindowAdapter >> okToChange [
^ self model okToChange
]

{ #category : #protocol }
MorphicWindowAdapter >> open [

	self model window openInWorld
]

{ #category : #protocol }
MorphicWindowAdapter >> openModal: aWindow [

	^ self widget 
		ifNotNil: [:w | w openModal: aWindow asWidget ].
]

{ #category : #protocol }
MorphicWindowAdapter >> rebuildWithSpec: aSpec [
	| sub |
	
	sub := self model window submorphs copy.
	self model window removeAllMorphs.
	sub allButLast do: [:e | self model window addMorphBack: e ].
	self model addModelIn: self widget withSpec: aSpec
]

{ #category : #protocol }
MorphicWindowAdapter >> rebuildWithSpecLayout: aSpec [
	| sub |
	
	sub := self model window submorphs copy.
	self model window removeAllMorphs.
	sub allButLast do: [:e | self model window addMorphBack: e ].
	self model addModelIn: self widget withSpecLayout: aSpec.
	self widget model: self
]

{ #category : #icons }
MorphicWindowAdapter >> taskbarIcon [ 
	^ self model taskbarIcon
]

{ #category : #'widget API' }
MorphicWindowAdapter >> title [
	^ self model title
]

{ #category : #protocol }
MorphicWindowAdapter >> title: aString [

	^ self widget
		ifNotNil: [:w | w title: aString ]
]

{ #category : #updating }
MorphicWindowAdapter >> windowIsClosing [

	self model windowIsClosing
]

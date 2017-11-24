"
I am the adpater used to bridge a TabManagerPresenter and a TabManager
"
Class {
	#name : #MorphicTabManagerAdapter,
	#superclass : #AbstractMorphicAdapter,
	#instVars : [
		'isClosedHolder'
	],
	#category : #'Spec-MorphicAdapters'
}

{ #category : #'spec protocol' }
MorphicTabManagerAdapter >> addTab: aTab [
	self widgetDo: [ :w | w addTab: (self buildTab: aTab) ]
]

{ #category : #factory }
MorphicTabManagerAdapter >> buildTab: aTab [
	^ aTab buildWithSpec
		when: #tabDeleted send: #tabDeleted: to: self;
		yourself
]

{ #category : #factory }
MorphicTabManagerAdapter >> buildWidget [
	^ TabManagerMorph new
		model: self model;
		setTabs: self getTabs;
		when: #tabSelected send: #tabSelected: to: self;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself
]

{ #category : #'widget API' }
MorphicTabManagerAdapter >> getTabs [
	^ [ (self model tabs collect: [ :each | self buildTab: each ]) collect: #asWidget ]
]

{ #category : #'spec protocol' }
MorphicTabManagerAdapter >> removeTab: aTab [
	"In Morphic, Tab removal is triggered on Tab, on TabManager."

	aTab widget delete
]

{ #category : #'widget API' }
MorphicTabManagerAdapter >> tabDeleted: aTab [
	"Tab removal notification triggered from Morphic world."

	self model basicRemoveTab: aTab model
]

{ #category : #'widget API' }
MorphicTabManagerAdapter >> tabSelected: aTab [

	self model selectedTab: aTab model
]

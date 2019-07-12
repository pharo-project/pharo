"
I am the adpater used to bridge a TabManagerPresenter and a TabManager
"
Class {
	#name : #SpMorphicTabManagerAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Deprecated-Adapters'
}

{ #category : #deprecation }
SpMorphicTabManagerAdapter class >> isDeprecated [
	^ true
]

{ #category : #'spec protocol' }
SpMorphicTabManagerAdapter >> addTab: aTab [
	self widgetDo: [ :w | w addTab: (self buildTab: aTab) ]
]

{ #category : #factory }
SpMorphicTabManagerAdapter >> buildTab: aTab [
	^ aTab buildWithSpec
		when: #tabDeleted send: #tabDeleted: to: self;
		yourself
]

{ #category : #factory }
SpMorphicTabManagerAdapter >> buildWidget [
	^ TabManagerMorph new
		model: self model;
		setTabs: self getTabs;
		when: #tabSelected send: #tabSelected: to: self;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself
]

{ #category : #'widget API' }
SpMorphicTabManagerAdapter >> getTabs [
	^ [ self model tabs collect: [ :each | self buildTab: each ] ]
]

{ #category : #'spec protocol' }
SpMorphicTabManagerAdapter >> removeTab: aTab [
	"In Morphic, Tab removal is triggered on Tab, on TabManager."

	aTab adapter delete
]

{ #category : #'widget API' }
SpMorphicTabManagerAdapter >> tabDeleted: aTab [
	"Tab removal notification triggered from Morphic world."

	self model basicRemoveTab: aTab model
]

{ #category : #'widget API' }
SpMorphicTabManagerAdapter >> tabSelected: aTab [

	self model selectedTab: aTab model
]

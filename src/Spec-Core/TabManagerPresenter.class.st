"
I am a spec presenter representing a tab manager. My role is to gather multiple tabs, and make them work together

You can also have a look at TabsExample for another example.

"
Class {
	#name : #TabManagerPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'tabs',
		'tabSelectedAction',
		'selectedTab'
	],
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
TabManagerPresenter class >> adapterName [

	^ #TabManagerAdapter
]

{ #category : #specs }
TabManagerPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(TabManagerAdapter
		adapt: #(model))
]

{ #category : #dependents }
TabManagerPresenter >> addDependent: aMorph [

	self myDependents add: aMorph
]

{ #category : #api }
TabManagerPresenter >> addTab: aTab [

	"BEWARE: this code is an exception. One should never access directly a value holder contents"
	"I do this here for optimisation. No need to trigger the whole collection changed. Only the added element is interesting "
	tabs value add: aTab.
	aTab owner: self.
	self selectedTab ifNil: [ selectedTab value: aTab ].
	self changed: #addTab: with: { aTab }
]

{ #category : #private }
TabManagerPresenter >> basicRemoveTab: aTab [
	"BEWARE: this code is an exception. One should never access directly a value holder contents"

	"I do this here for optimisation. No need to trigger the whole collection changed. Only the added element is interesting "

	"This method simply removes the tab from the collection without triggering further updates. It is useful for Adapters if there are different mechanisms of tab removal."

	(tabs value remove: aTab ifAbsent: [ ^ self ]) owner: nil
]

{ #category : #initialization }
TabManagerPresenter >> initialize [
	"Initialization code for TabManagerPresenter"

	super initialize.
	tabs := OrderedCollection new asValueHolder.
	tabSelectedAction := [  ] asValueHolder.
	selectedTab := nil asValueHolder.
	self myDependents: WeakSet new.

	tabs whenChangedDo: [ :new | self widget ifNotNil: [ :w | w tabs: self getTabs value ] ].
	selectedTab whenChangedDo: [ :newTab | tabSelectedAction value cull: newTab cull: self ]
]

{ #category : #api }
TabManagerPresenter >> removeTab: aTab [
	self basicRemoveTab: aTab.
	self changed: #removeTab: with: {aTab}
]

{ #category : #api }
TabManagerPresenter >> removeTabNumber: anIndex [

	self removeTab: (tabs at: anIndex)
]

{ #category : #api }
TabManagerPresenter >> selectTabModel: tabModel [

	tabSelectedAction value cull: tabModel cull: self
]

{ #category : #api }
TabManagerPresenter >> selectedTab [

	^ selectedTab value
]

{ #category : #api }
TabManagerPresenter >> selectedTab: aTab [

	selectedTab value: aTab
]

{ #category : #api }
TabManagerPresenter >> tabSelectedAction [

	^ tabSelectedAction value
]

{ #category : #accessing }
TabManagerPresenter >> tabs [

	^ tabs value
]

{ #category : #api }
TabManagerPresenter >> tabs: aCollection [

	tabs value: aCollection
]

{ #category : #api-events }
TabManagerPresenter >> whenTabSelected: aBlock [

	tabSelectedAction value: aBlock
]

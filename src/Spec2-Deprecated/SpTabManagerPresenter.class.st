"
I am a spec presenter representing a tab manager. My role is to gather multiple tabs, and make them work together

You can also have a look at TabsExample for another example.

"
Class {
	#name : #SpTabManagerPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'tabs',
		'tabSelectedAction',
		'selectedTab'
	],
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #specs }
SpTabManagerPresenter class >> adapterName [

	^ #TabManagerAdapter
]

{ #category : #deprecation }
SpTabManagerPresenter class >> isDeprecated [
	^ true
]

{ #category : #dependents }
SpTabManagerPresenter >> addDependent: aMorph [

	self myDependents add: aMorph
]

{ #category : #api }
SpTabManagerPresenter >> addTab: aTab [

	"BEWARE: this code is an exception. One should never access directly a value holder contents"
	"I do this here for optimisation. No need to trigger the whole collection changed. Only the added element is interesting "
	tabs value add: aTab.
	aTab owner: self.
	self selectedTab ifNil: [ selectedTab value: aTab ].
	self changed: #addTab: with: { aTab }
]

{ #category : #private }
SpTabManagerPresenter >> basicRemoveTab: aTab [
	"BEWARE: this code is an exception. One should never access directly a value holder contents"

	"I do this here for optimisation. No need to trigger the whole collection changed. Only the added element is interesting "

	"This method simply removes the tab from the collection without triggering further updates. It is useful for Adapters if there are different mechanisms of tab removal."

	(tabs value remove: aTab ifAbsent: [ ^ self ]) owner: nil
]

{ #category : #initialization }
SpTabManagerPresenter >> initialize [
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
SpTabManagerPresenter >> removeTab: aTab [
	self basicRemoveTab: aTab.
	self changed: #removeTab: with: {aTab}
]

{ #category : #api }
SpTabManagerPresenter >> removeTabNumber: anIndex [

	self removeTab: (tabs at: anIndex)
]

{ #category : #api }
SpTabManagerPresenter >> selectTabModel: tabModel [

	tabSelectedAction value cull: tabModel cull: self
]

{ #category : #api }
SpTabManagerPresenter >> selectedTab [

	^ selectedTab value
]

{ #category : #api }
SpTabManagerPresenter >> selectedTab: aTab [

	selectedTab value: aTab
]

{ #category : #api }
SpTabManagerPresenter >> tabSelectedAction [

	^ tabSelectedAction value
]

{ #category : #accessing }
SpTabManagerPresenter >> tabs [

	^ tabs value
]

{ #category : #api }
SpTabManagerPresenter >> tabs: aCollection [

	tabs value: aCollection
]

{ #category : #'api-events' }
SpTabManagerPresenter >> whenTabSelected: aBlock [

	tabSelectedAction value: aBlock
]

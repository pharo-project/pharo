"
I am a spec presenter representing a single tab.

I should not be used without being contained into a TabManagerPresenter.

You can also have a look at TabsExample for another example.

!! #retrievingBlock: warning

If you specify #retrievingBlock: directly, you should also set the ownership of the appropriate. (See TabPresenter>>#model:)
"
Class {
	#name : #TabPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'tabSelectedActionHolder',
		'labelHolder',
		'iconHolder',
		'morphHolder',
		'retrievingBlockHolder',
		'menuHolder',
		'actionsHolder',
		'closeableHolder',
		'selectedHolder'
	],
	#category : #Spec-Core-Widgets
}

{ #category : #specs }
TabPresenter class >> adapterName [

	^ #TabAdapter
]

{ #category : #specs }
TabPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(TabAdapter
		adapt: #(model))
]

{ #category : #api }
TabPresenter >> actions [

	^ actionsHolder value
]

{ #category : #api }
TabPresenter >> actions: aCollection [

	actionsHolder value: aCollection
]

{ #category : #api-valueHolder }
TabPresenter >> actionsHolder [

	^ actionsHolder
]

{ #category : #api }
TabPresenter >> closeable [

	^ closeableHolder value
]

{ #category : #api }
TabPresenter >> closeable: aBoolean [

	closeableHolder value: aBoolean
]

{ #category : #api-valueHolder }
TabPresenter >> closeableHolder [

	^ closeableHolder
]

{ #category : #api }
TabPresenter >> contents [

	^ morphHolder value
]

{ #category : #api }
TabPresenter >> contents: aMorph [

	morphHolder value: aMorph
]

{ #category : #private }
TabPresenter >> defaultIcon [

	^ self iconNamed: #blankIcon
]

{ #category : #api }
TabPresenter >> icon [

	^ iconHolder value
]

{ #category : #api }
TabPresenter >> icon: aForm [

	iconHolder value: aForm
]

{ #category : #api-valueHolder }
TabPresenter >> iconHolder [

	^ iconHolder
]

{ #category : #api }
TabPresenter >> iconName: aSymbol [

	iconHolder value: (self iconNamed: aSymbol)
]

{ #category : #initialization }
TabPresenter >> initialExtent [

	^ 200@25
]

{ #category : #initialization }
TabPresenter >> initialize [
	"Initialization code for TabPresenter"

	super initialize.

	labelHolder := '' asValueHolder.
	iconHolder := self defaultIcon asValueHolder.
	morphHolder := nil asValueHolder.
	retrievingBlockHolder := nil asValueHolder.
	menuHolder := nil asValueHolder.
	actionsHolder := OrderedCollection new asValueHolder.
	closeableHolder := true asValueHolder.
	tabSelectedActionHolder := [  ] asValueHolder.
	selectedHolder := false asValueHolder
]

{ #category : #api }
TabPresenter >> label [

	^ labelHolder value
]

{ #category : #api }
TabPresenter >> label: aString [

	labelHolder value: aString
]

{ #category : #api-valueHolder }
TabPresenter >> labelHolder [

	^ labelHolder
]

{ #category : #api }
TabPresenter >> menu [

	^ menuHolder value
]

{ #category : #api }
TabPresenter >> menu: aBlock [

	menuHolder value: aBlock
]

{ #category : #api-valueHolder }
TabPresenter >> menuHolder [

	^ menuHolder
]

{ #category : #api }
TabPresenter >> model: aComposablePresenter [
	aComposablePresenter owner: self.
	self retrievingBlock: [ aComposablePresenter buildWithSpec asWidget ]
]

{ #category : #api-valueHolder }
TabPresenter >> morphHolder [

	^ morphHolder
]

{ #category : #api }
TabPresenter >> retrievingBlock [

	^ retrievingBlockHolder value
]

{ #category : #api }
TabPresenter >> retrievingBlock: aBlock [

	retrievingBlockHolder value: aBlock
]

{ #category : #api-valueHolder }
TabPresenter >> retrievingBlockHolder [

	^ retrievingBlockHolder
]

{ #category : #api }
TabPresenter >> selected: aBoolean [

	selectedHolder value: aBoolean
]

{ #category : #api-valueHolder }
TabPresenter >> selectedHolder [

	^ selectedHolder
]

{ #category : #api }
TabPresenter >> tabSelected [ 

	tabSelectedActionHolder value cull: self
]

{ #category : #api }
TabPresenter >> tabSelectedAction [

	^ tabSelectedActionHolder value
]

{ #category : #api-valueHolder }
TabPresenter >> tabSelectedActionHolder [

	^ tabSelectedActionHolder
]

{ #category : #api-events }
TabPresenter >> whenTabSelected: aBlock [

	tabSelectedActionHolder value: aBlock
]

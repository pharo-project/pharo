"
I am a spec presenter representing a single tab.

I should not be used without being contained into a TabManagerPresenter.

You can also have a look at TabsExample for another example.

!! #retrievingBlock: warning

If you specify #retrievingBlock: directly, you should also set the ownership of the appropriate. (See TabPresenter>>#model:)
"
Class {
	#name : #SpTabPresenter,
	#superclass : #SpAbstractWidgetPresenter,
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
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #specs }
SpTabPresenter class >> adapterName [

	^ #TabAdapter
]

{ #category : #deprecation }
SpTabPresenter class >> isDeprecated [
	^ true
]

{ #category : #api }
SpTabPresenter >> actions [

	^ actionsHolder value
]

{ #category : #api }
SpTabPresenter >> actions: aCollection [

	actionsHolder value: aCollection
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> actionsHolder [

	^ actionsHolder
]

{ #category : #api }
SpTabPresenter >> closeable [

	^ closeableHolder value
]

{ #category : #api }
SpTabPresenter >> closeable: aBoolean [

	closeableHolder value: aBoolean
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> closeableHolder [

	^ closeableHolder
]

{ #category : #api }
SpTabPresenter >> contents [

	^ morphHolder value
]

{ #category : #api }
SpTabPresenter >> contents: aMorph [

	morphHolder value: aMorph
]

{ #category : #private }
SpTabPresenter >> defaultIcon [

	^ self iconNamed: #blankIcon
]

{ #category : #api }
SpTabPresenter >> icon [

	^ iconHolder value
]

{ #category : #api }
SpTabPresenter >> icon: aForm [

	iconHolder value: aForm
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> iconHolder [

	^ iconHolder
]

{ #category : #api }
SpTabPresenter >> iconName: aSymbol [

	iconHolder value: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpTabPresenter >> initialExtent [

	^ 200@25
]

{ #category : #initialization }
SpTabPresenter >> initialize [
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
SpTabPresenter >> label [

	^ labelHolder value
]

{ #category : #api }
SpTabPresenter >> label: aString [

	labelHolder value: aString
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> labelHolder [

	^ labelHolder
]

{ #category : #api }
SpTabPresenter >> menu [

	^ menuHolder value
]

{ #category : #api }
SpTabPresenter >> menu: aBlock [

	menuHolder value: aBlock
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> menuHolder [

	^ menuHolder
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> morphHolder [

	^ morphHolder
]

{ #category : #api }
SpTabPresenter >> presenter: aPresenter [
	aPresenter owner: self.
	self retrievingBlock: [ aPresenter buildWithSpec ]
]

{ #category : #api }
SpTabPresenter >> retrievingBlock [

	^ retrievingBlockHolder value
]

{ #category : #api }
SpTabPresenter >> retrievingBlock: aBlock [

	retrievingBlockHolder value: aBlock
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> retrievingBlockHolder [

	^ retrievingBlockHolder
]

{ #category : #api }
SpTabPresenter >> selected: aBoolean [

	selectedHolder value: aBoolean
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> selectedHolder [

	^ selectedHolder
]

{ #category : #api }
SpTabPresenter >> tabSelected [ 

	tabSelectedActionHolder value cull: self
]

{ #category : #api }
SpTabPresenter >> tabSelectedAction [

	^ tabSelectedActionHolder value
]

{ #category : #'api-valueHolder' }
SpTabPresenter >> tabSelectedActionHolder [

	^ tabSelectedActionHolder
]

{ #category : #'api-events' }
SpTabPresenter >> whenTabSelected: aBlock [

	tabSelectedActionHolder value: aBlock
]

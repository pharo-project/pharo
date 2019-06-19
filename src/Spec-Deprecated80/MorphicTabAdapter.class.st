"
I am the adapter used to bridge a TabPresenter and a Tab
"
Class {
	#name : #MorphicTabAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-Deprecated80-Adapters'
}

{ #category : #deprecation }
MorphicTabAdapter class >> isDeprecated [
	^ true
]

{ #category : #initialization }
MorphicTabAdapter >> adapt: aComposablePresenter [
	widget := self buildWidgetWith: aComposablePresenter.
	aComposablePresenter addDependent: widget.
	self bindModel: aComposablePresenter andWidget: widget
]

{ #category : #updating }
MorphicTabAdapter >> bindModel: aTabModel andWidget: aWidget [
	"bind change propagation between Spec TabPresenter and Morphic Tab"

	aWidget when: #tabSelected send: #tabSelected to: aTabModel.
	aTabModel actionsHolder whenChangedDo: [ :new | aWidget actions: new ].
	aTabModel closeableHolder whenChangedDo: [ :new | aWidget closeable: new ].
	aTabModel iconHolder whenChangedDo: [ :new | aWidget icon: new ].
	aTabModel labelHolder whenChangedDo: [ :new | aWidget label: new ].
	aTabModel menuHolder whenChangedDo: [ :new | aWidget menu: new ].
	aTabModel morphHolder whenChangedDo: [ :new | aWidget morph: new ].
	aTabModel retrievingBlockHolder whenChangedDo: [ :new | aWidget retrievingBlock: new ].
	aTabModel selectedHolder whenChangedDo: [ :new | aWidget selected: new ]
]

{ #category : #factory }
MorphicTabAdapter >> buildWidgetWith: aTabPresenter [
	^ TabMorph new
		model: aTabPresenter;
		label: aTabPresenter label;
		icon: aTabPresenter icon;
		retrievingBlock: aTabPresenter retrievingBlock;
		morph: aTabPresenter contents;
		menu: aTabPresenter menu;
		closeable: aTabPresenter closeable;
		setActionsFrom: [ aTabPresenter actions ];
		yourself
]

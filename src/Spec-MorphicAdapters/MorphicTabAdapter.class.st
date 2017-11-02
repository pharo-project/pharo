"
I am the adapter used to bridge a TabPresenter and a Tab
"
Class {
	#name : #MorphicTabAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : 'Spec-MorphicAdapters'
}

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
MorphicTabAdapter >> buildWidgetWith: aTabModel [
	^ TabMorph new
		model: aTabModel;
		label: aTabModel label;
		icon: aTabModel icon;
		retrievingBlock: aTabModel retrievingBlock;
		morph: aTabModel contents;
		menu: aTabModel menu;
		closeable: aTabModel closeable;
		setActionsFrom: [ aTabModel actions ];
		yourself
]

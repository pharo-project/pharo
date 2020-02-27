"
I am the adapter used to bridge a TabPresenter and a Tab
"
Class {
	#name : #SpMorphicTabAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Deprecated-Adapters'
}

{ #category : #deprecation }
SpMorphicTabAdapter class >> isDeprecated [
	^ true
]

{ #category : #initialization }
SpMorphicTabAdapter >> adapt: aPresenter [
	widget := self buildWidgetWith: aPresenter.
	aPresenter addDependent: widget.
	self bindModel: aPresenter andWidget: widget
]

{ #category : #updating }
SpMorphicTabAdapter >> bindModel: aTabModel andWidget: aWidget [
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
SpMorphicTabAdapter >> buildWidgetWith: aTabPresenter [
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

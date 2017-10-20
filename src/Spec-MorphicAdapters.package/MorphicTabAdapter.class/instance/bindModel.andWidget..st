updating
bindModel: aTabModel andWidget: aWidget
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
initialization
initializePresenter
	changesTree whenHighlightedItemChanged: [ :item | item ifNotNil: [ textArea text: (self buildDiffFor: item content) ] ]
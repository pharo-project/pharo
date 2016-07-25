actions
performActiveTabAction
	"Look up the Symbol or Block associated with the currently active tab, and perform it."
	
	| tabActionAssoc aSymbolOrBlock |
	
	tabActionAssoc := self tabs detect: [ :assoc | assoc key = self activeTab.] ifNone: [ Association new ].
	aSymbolOrBlock := tabActionAssoc value.
	aSymbolOrBlock ifNil: [ ^ false ].
	^ aSymbolOrBlock isSymbol
		ifTrue: [ self target perform: aSymbolOrBlock ]
		ifFalse: [ aSymbolOrBlock value ].
	
label
update: aSymbol
	aSymbol = #relabel
		ifTrue: [^ model ifNotNil: [ self setLabel: model labelString ] ].
	aSymbol = #close
		ifTrue: [self delete]
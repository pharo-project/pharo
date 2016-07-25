asserting
assertMethodWidgetAdaptor
	| index item |
	"test result changed because nautilus behavior has changed.
	issue 14890 (nhe)"
	index := nautilus ui methodWidget methodsList selectedRowIndex.
	nautilus selectedMethod ifNotNil: [ 
		self assert: index notNil.
		self deny: index = 0.
		item := nautilus ui methodWidget methodsList dataSource elementAt: index.
		self assert: item equals: compiledMethod ]
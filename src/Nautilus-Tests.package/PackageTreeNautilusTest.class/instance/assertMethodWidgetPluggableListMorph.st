asserting
assertMethodWidgetPluggableListMorph
	| index item |
	"test result changed because nautilus behavior has changed.
	issue 14890 (nhe)"
	self assert: nautilus ui methodWidget methodsList listMorph submorphs notEmpty.
	index := nautilus ui methodWidget methodsList listMorph selectedRow.
	nautilus selectedMethod ifNotNil: [ 
		self assert: index notNil.
		item := nautilus ui methodWidget methodsList listMorph item: index.
		self assert: item originalObject equals: compiledMethod ]
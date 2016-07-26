asserting
assertBundle
	bundle := packing pack: collector.
	self assert: bundle url equals: collector url.
	self assert: bundle data isLiteral.
	self assert: bundle category equals: collector category.
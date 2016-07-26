visitor
visitSingleBundle: aSingleBundle
	| multiBundle |
	multiBundle := multiBundles 
							at: aSingleBundle url 
							ifAbsentPut: [ GTEventMultiBundle new url: aSingleBundle url; yourself ].
	multiBundle add: aSingleBundle
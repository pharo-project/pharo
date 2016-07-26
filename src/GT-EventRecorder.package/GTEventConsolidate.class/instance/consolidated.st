accessing - computed
consolidated
	| consolidatedCollectors |
	consolidatedCollectors := OrderedDictionary new.
	collectors do: [ :eachCollector | | aCollector |
		aCollector := consolidatedCollectors 
								at: (eachCollector url printString, '/', eachCollector category) asSymbol
								ifAbsentPut: [ GTEventCollector new 
														url: eachCollector url; 
														category: eachCollector category;
														yourself ].
		eachCollector bundle do: [ :eachEvent | aCollector add: eachEvent ] ].
	^ consolidatedCollectors asArray
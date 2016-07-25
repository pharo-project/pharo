packing
pack
	| allCollectors consolidatedCollectors |
	self canSendDiagnosticsAndUsageData ifFalse: [ ^ self flush; yourself ].
	allCollectors := OrderedCollection new: collectors size.
	collectors do: [ :eachCollector | allCollectors addLast: eachCollector ].
	consolidatedCollectors := GTEventConsolidate new collectors: allCollectors; consolidated.
	consolidatedCollectors do: [ :eachCollector | | bundle |
		eachCollector size > 0 ifTrue: [ 
			bundle := packing pack: eachCollector.
			delivery add: bundle] ].
	self removeLiberatedCollectors.
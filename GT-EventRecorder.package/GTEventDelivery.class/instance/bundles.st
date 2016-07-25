delivery
bundles
	"Be careful about using this method. You will loose data if you call it externaly."
	| eventBundles |
	eventBundles := OrderedCollection new.
	queue flush: [ :eachGTEventBundle | eventBundles addLast: eachGTEventBundle ].
	^ eventBundles
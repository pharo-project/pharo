public
fromBundles: aCollection 
	multiBundles := OrderedDictionary new.
	aCollection do: [ :aSingleOrMultiBundle | self synthetize: aSingleOrMultiBundle.].
	^ self multiBundles
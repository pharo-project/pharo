private
lookupInPools: varName ifFound: assocBlock

	^Symbol
		hasInterned: varName
		ifTrue:
			[:sym|
			(class bindingOf: sym)
				ifNil: [^false]
				ifNotNil: [:assoc| assocBlock value: assoc]]
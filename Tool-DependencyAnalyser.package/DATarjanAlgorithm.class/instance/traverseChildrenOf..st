private
traverseChildrenOf: aPDPackage
	aPDPackage dependencies
		do: [ :dependency | 
			dependency target isTarjanUndefined
				ifTrue: [ 
					self traverse: dependency target.
					aPDPackage tarjanLowLink: (aPDPackage tarjanLowLink min: dependency target tarjanLowLink) ]
				ifFalse: [ 
					dependency target isInStack
						ifTrue: [ aPDPackage tarjanLowLink: (aPDPackage tarjanLowLink min: dependency target tarjanIndex) ] ] ]
private
needsMethodSignatureOnMultipleLinesFor: aMethodNode
	| cpt |
	cpt := 0.
	self methodSignatureOnMultipleLines
		ifTrue:
			[ 
			aMethodNode selectorParts
				with: aMethodNode arguments
				do:
					[ :key :arg | 
					key size + arg name size > self selectorAndArgumentCombinedMaxSize
						ifTrue: [ cpt := cpt + 1 ] ].
			^ cpt > 1 ]
		ifFalse: [ ^ false ]
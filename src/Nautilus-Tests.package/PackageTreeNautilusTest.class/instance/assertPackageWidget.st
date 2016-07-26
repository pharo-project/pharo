asserting
assertPackageWidget
	| packageNodeList packageMorphList |
	packageNodeList := nautilus ui packageWidget packagesList nodeList
		select: [ :eachNode | eachNode item = compiledMethod package ].
	packageMorphList := nautilus ui packageWidget packagesList scroller submorphs
		select: [ :eachMorph | packageNodeList anySatisfy: [ :eachNode | eachMorph complexContents = eachNode ] ].
	self assert: packageNodeList size equals: packageMorphList size.
	self assert: packageNodeList size <= 1.
	packageMorphList do: [ :eachMorph | self assertPackageMorph: eachMorph ]
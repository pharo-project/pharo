ui requests
openComparisonFrom: targetMethodSource
					to: originalMethodSource
					belongingTo: aClass
					from: aChange 
					labeled: aLabel 
					inWindowLabeled: aWindowLabel 
	| diffBuilder difference  |
	PSMCPatchMorph usedByDefault 
		ifTrue: [ ^ self openPolymorphComparisonFrom: originalMethodSource 
					to: targetMethodSource
					belongingTo: aClass
					from: aChange
					labeled: aLabel
					inWindowLabeled: aWindowLabel   ].
	diffBuilder :=  TextDiffBuilder
			from: originalMethodSource 
			to: targetMethodSource.
	difference := diffBuilder buildDisplayPatch.
	self edit: difference label: aLabel.
				
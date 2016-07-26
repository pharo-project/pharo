private
openPolymorphComparisonFrom: targetMethodSource
					to: originalMethodSource
					belongingTo: aClass
					from: aChange 
					labeled: aLabel 
					inWindowLabeled: aWindowLabel 
	| diffMorph |
	diffMorph := DiffChangeMorph
		from: targetMethodSource
		label: aChange stamp
		to: originalMethodSource
		label: (aClass compiledMethodAt: aChange methodSelector) timeStamp
		contextClass: aClass.
	diffMorph openInWindowLabeled: aWindowLabel.
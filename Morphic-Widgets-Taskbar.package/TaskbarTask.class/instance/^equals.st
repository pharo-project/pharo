comparing
= aTaskbarTask
	"Answer whether equal."

	^self species = aTaskbarTask species
		and: [self morph == aTaskbarTask morph
		and: [self state == aTaskbarTask state
		and: [self icon = aTaskbarTask icon
		and: [self label = aTaskbarTask label]]]]